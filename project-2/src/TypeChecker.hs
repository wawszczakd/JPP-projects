{-# LANGUAGE FlexibleContexts #-}

module TypeChecker where
    import AbsLagerLang
    import Control.Monad.Reader
    import Control.Monad.State
    import Control.Monad.Except
    import Data.Map as Map
    import qualified Data.List as List
    import Text.ParserCombinators.ReadP (get)
    
    showPosition :: Maybe (Int, Int) -> String
    showPosition Nothing = "position unknown"
    showPosition (Just (row, col)) = "row: " ++ (show row) ++ ", column: " ++ (show col)
    
    data MyType = MyInt | MyStr | MyBool | MyVoid | MyFun MyType [MyType] deriving (Eq, Show)
    toMyType :: Type -> MyType
    toMyType (Int _) = MyInt
    toMyType (Str _) = MyStr
    toMyType (Bool _) = MyBool
    toMyType (Void _) = MyVoid
    toMyType (Fun _ typ args) = MyFun (toMyType typ) (Prelude.map toMyType args)
    
    toMyTypeArg :: Arg -> MyType
    toMyTypeArg (ValArg _ typ _) = toMyType typ
    toMyTypeArg (RefArg _ typ _) = toMyType typ
    
    type Env = Map.Map Ident MyType
    
    type TypeCheckerMonad = ReaderT Env (ExceptT String IO)
    
    typeCheck :: Program -> TypeCheckerMonad ()
    typeCheck (Prog _ xs) =
        go xs
        where
            go :: [TopDef] -> TypeCheckerMonad ()
            go [] = return ()
            go (x:xs) = do
                env <- checkTopDef x
                local (const env) (go xs)
    
    checkArgsNames :: Maybe (Int, Int) -> [Arg] -> Bool
    checkArgsNames pos args =
        let args' = Prelude.map (\arg -> case arg of
                                    ValArg _ _ name -> name
                                    RefArg _ _ name -> name) args
        in if List.nub args' == args' then True
        else False
    
    checkTopDef :: TopDef -> TypeCheckerMonad Env
    checkTopDef (FnDef pos typ name args block) = do
        env <- ask
        if not (checkArgsNames pos args) then
            throwError ("Function args names must be pairwise distinct, " ++ (showPosition pos))
        else do
            let typ' = toMyType typ
                args' = Prelude.map toMyTypeArg args
            return $ Map.insert name (MyFun typ' args') env
    
    checkTopDef (VarDef _ typ name) = do
        env <- ask
        return $ Map.insert name (toMyType typ) env
    
    checkTopDef (VarDefAss pos typ name expr) = do
        env <- ask
        let typ' = toMyType typ
        exprType <- getExprType expr
        if typ' == exprType then
            return $ Map.insert name typ' env
        else
            throwError ("Wrong type" ++ (showPosition pos))
    
    getExprType :: Expr -> TypeCheckerMonad MyType
    getExprType (EVar pos name) = getTypeFromEnv pos name
    getExprType (ELitInt _ _) = return MyInt
    getExprType (ELitTrue _) = return MyBool
    getExprType (ELitFalse _) = return MyBool
    getExprType (EString _ _) = return MyStr
    
    getExprType (EApp pos name args) = do
        tmp <- getTypeFromEnv pos name
        case tmp of
            MyFun typ argsTypes -> do
                argsTypes' <- mapM getExprType args
                if argsTypes == argsTypes' then return typ
                else throwError ("Wrong type, " ++ (showPosition pos))
            _ -> throwError ("Wrong type, " ++ (showPosition pos))
    
    getExprType (Neg pos expr) = do
        exprType <- getExprType expr
        case exprType of
            MyInt -> return MyInt
            _ -> throwError ("'-' requires operand to be Int, " ++ (showPosition pos))
    
    getExprType (Not pos expr) = do
        exprType <- getExprType expr
        case exprType of
            MyBool -> return MyBool
            _ -> throwError ("! requires operand to be Bool, " ++ (showPosition pos))
    
    getExprType (EMul pos expr1 op expr2) = do
        exprType1 <- getExprType expr1
        exprType2 <- getExprType expr2
        case op of
            Times _ -> handleMul exprType1 exprType2
            Div _   -> handleDiv exprType1 exprType2
            Mod _   -> handleMod exprType1 exprType2
        where
            handleMul MyInt MyInt = return MyInt
            handleMul _ _ = throwError ("'*' requires both operands to be Ints, " ++ (showPosition pos))
            handleDiv MyInt MyInt = return MyInt
            handleDiv _ _ = throwError ("'/' requires both operands to be Ints, " ++ (showPosition pos))
            handleMod MyInt MyInt = return MyInt
            handleMod _ _ = throwError ("'%' requires both operands to be Ints, " ++ (showPosition pos))
    
    getExprType (EAdd pos expr1 op expr2) = do
        exprType1 <- getExprType expr1
        exprType2 <- getExprType expr2
        case op of
            Plus _  -> handleAdd exprType1 exprType2
            Minus _ -> handleSub exprType1 exprType2
        where
            handleAdd MyInt MyInt = return MyInt
            handleAdd _ _ = throwError ("'+' requires both operands to be Ints, " ++ (showPosition pos))
            handleSub MyInt MyInt = return MyInt
            handleSub _ _ = throwError ("'-' requires both operands to be Ints, " ++ (showPosition pos))
    
    getExprType (ERel pos expr1 op expr2) = do
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
            handleComparison _ _ = throwError ("Comparison requires operands of compatible types, " ++ (showPosition pos))
            handleEquality ty1 ty2
                | ty1 == ty2 = return MyBool
                | otherwise = throwError ("Equality requires operands of the same type, " ++ (showPosition pos))
    
    getExprType (EAnd pos expr1 expr2) = do
        exprType1 <- getExprType expr1
        exprType2 <- getExprType expr2
        handleLogicalOp pos exprType1 exprType2
    
    getExprType (EOr pos expr1 expr2) = do
        exprType1 <- getExprType expr1
        exprType2 <- getExprType expr2
        handleLogicalOp pos exprType1 exprType2
    
    handleLogicalOp :: Maybe (Int, Int) -> MyType -> MyType -> TypeCheckerMonad MyType
    handleLogicalOp pos MyBool MyBool = return MyBool
    handleLogicalOp pos _ _ = throwError ("Logical operations require operands of type Bool, " ++ (showPosition pos))
    
    getTypeFromEnv :: Maybe (Int, Int) -> Ident -> TypeCheckerMonad MyType
    getTypeFromEnv pos (Ident name) = do
        env <- ask
        case Map.lookup (Ident name) env of
            Just typ -> return typ
            Nothing -> throwError (name ++ " is not defined" ++ (showPosition pos))
