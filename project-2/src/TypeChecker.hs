{-# LANGUAGE FlexibleContexts #-}

module TypeChecker where
    import AbsLagerLang
    import Control.Monad.Reader
    import Control.Monad.State
    import Control.Monad.Except
    import Data.Map as Map
    import Text.ParserCombinators.ReadP (get)
    
    type Env = Map.Map Ident Type
    
    type TypeCheckerMonad = ReaderT Env (ExceptT String IO)
    
    data MyType = MyInt | MyStr | MyBool | MyVoid deriving Eq
    toMyType :: Type -> MyType
    toMyType (Int _) = MyInt
    toMyType (Str _) = MyStr
    toMyType (Bool _) = MyBool
    toMyType (Void _) = MyVoid
    toMyType (Fun _ typ _) = toMyType typ
    
    typeCheck :: Program -> TypeCheckerMonad ()
    typeCheck (Prog pos xs) =
        go Map.empty xs
        where
            go :: Env -> [TopDef] -> TypeCheckerMonad ()
            go _ [] = return ()
            go env (x:xs) = do
                env' <- check x env
                go env' xs
    
    check :: TopDef -> Env -> TypeCheckerMonad Env
    check (FnDef _ typ name args block) env = return env
    check (VarDef _ typ name) env = return env
    check (VarDefAss _ typ name expr) env = do
        let typ' = toMyType typ
        exprType <- getExprType expr
        if typ' == exprType then
            return $ Map.insert name typ env
        else
            throwError "Wrong type"
    
    getExprType :: Expr -> TypeCheckerMonad MyType
    getExprType (EVar _ name) = getVarType name
    getExprType (ELitInt _ _) = return MyInt
    getExprType (ELitTrue _) = return MyBool
    getExprType (ELitFalse _) = return MyBool
    getExprType (EApp _ name _) = getVarType name
    getExprType (EString _ _) = return MyStr
    getExprType (Neg _ expr) = do
        exprType <- getExprType expr
        case exprType of
            MyInt -> return MyInt
            _ -> throwError "'-' requires operand to be Int"
    getExprType (Not _ expr) = do
        exprType <- getExprType expr
        case exprType of
            MyBool -> return MyBool
            _ -> throwError "! requires operand to be Bool"
    getExprType (EMul _ expr1 op expr2) = do
        exprType1 <- getExprType expr1
        exprType2 <- getExprType expr2
        case op of
            Times _ -> handleMul exprType1 exprType2
            Div _   -> handleDiv exprType1 exprType2
            Mod _   -> handleMod exprType1 exprType2
        where
            handleMul MyInt MyInt = return MyInt
            handleMul _ _ = throwError "'*' requires both operands to be Ints"
            handleDiv MyInt MyInt = return MyInt
            handleDiv _ _ = throwError "'/' requires both operands to be Ints"
            handleMod MyInt MyInt = return MyInt
            handleMod _ _ = throwError "'%' requires both operands to be Ints"
    getExprType (EAdd _ expr1 op expr2) = do
        exprType1 <- getExprType expr1
        exprType2 <- getExprType expr2
        case op of
            Plus _  -> handleAdd exprType1 exprType2
            Minus _ -> handleSub exprType1 exprType2
        where
            handleAdd MyInt MyInt = return MyInt
            handleAdd _ _ = throwError "'+' requires both operands to be Ints"
            handleSub MyInt MyInt = return MyInt
            handleSub _ _ = throwError "'-' requires both operands to be Ints"
    
    getExprType (ERel _ expr1 op expr2) = do
        exprType1 <- getExprType expr1
        exprType2 <- getExprType expr2
        case op of
            LTH _  -> handleComparison exprType1 exprType2
            LE _   -> handleComparison exprType1 exprType2
            GTH _  -> handleComparison exprType1 exprType2
            GE _   -> handleComparison exprType1 exprType2
            EQU _  -> handleEquality exprType1 exprType2
            NE _   -> handleEquality exprType1 exprType2
        where
            handleComparison MyInt MyInt = return MyBool
            handleComparison MyStr MyStr = return MyBool
            handleComparison MyBool MyBool = return MyBool
            handleComparison _ _ = throwError "Comparison requires operands of compatible types"
            handleEquality ty1 ty2
                | ty1 == ty2 = return MyBool
                | otherwise = throwError "Equality requires operands of the same type"
    
    getExprType (EAnd _ expr1 expr2) = do
        exprType1 <- getExprType expr1
        exprType2 <- getExprType expr2
        handleLogicalOp exprType1 exprType2
    
    getExprType (EOr _ expr1 expr2) = do
        exprType1 <- getExprType expr1
        exprType2 <- getExprType expr2
        handleLogicalOp exprType1 exprType2
    
    handleLogicalOp :: MyType -> MyType -> TypeCheckerMonad MyType
    handleLogicalOp MyBool MyBool = return MyBool
    handleLogicalOp _ _ = throwError "Logical operations require operands of type Bool"
    
    getVarType :: Ident -> TypeCheckerMonad MyType
    getVarType (Ident name) = do
        env <- ask
        case Map.lookup (Ident name) env of
            Just typ -> return $ toMyType typ
            Nothing -> throwError (name ++ " is not defined")
