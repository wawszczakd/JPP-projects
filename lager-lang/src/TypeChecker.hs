{-# LANGUAGE FlexibleContexts #-}

module TypeChecker where
    import AbsLagerLang
    import Common
    import Control.Monad.Reader
    import Control.Monad.State
    import Control.Monad.Except
    import Data.Map as Map
    import Data.Maybe
    import qualified Data.List as List
    import Text.ParserCombinators.ReadP (get)
    
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
    
    type Env = (Map.Map Ident MyType, Bool)
    
    type TypeCheckerMonad = ReaderT Env (ExceptT String IO)
    
    typeCheck :: Program -> TypeCheckerMonad ()
    typeCheck (Prog _ xs) =
        go xs
        where
            go :: [TopDef] -> TypeCheckerMonad ()
            go [] = do
                (env, _) <- ask
                let tmp = Map.lookup (Ident "main") env
                case tmp of
                    Just (MyFun MyInt []) -> return ()
                    _ -> throwError "'main' must be a no-argument function that returns int"
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
        (env, isLoop) <- ask
        if not (checkArgsNames pos args) then
            throwError ("Function args names must be pairwise distinct, " ++ (showPosition pos))
        else do
            let typ' = toMyType typ
                argTypes = Prelude.map (\arg -> toMyTypeArg arg) args
            if elem MyVoid argTypes then do
                throwError ("Argument cannot be of type void, " ++ (showPosition pos))
            else do
                let env' = Map.insert name (MyFun typ' argTypes) env
                    insertArgToEnv env'' (ValArg _ typ argIdent) = Map.insert argIdent (toMyType typ) env''
                    insertArgToEnv env'' (RefArg _ typ argIdent) = Map.insert argIdent (toMyType typ) env''
                    envWithArgs = Prelude.foldl insertArgToEnv env' args
                (_, typ'') <- local (const (envWithArgs, False)) (checkBlock block)
                case typ'' of
                    Nothing -> do
                        if typ' == MyVoid then
                            return (env', isLoop)
                        else
                            throwError ("No return in non-void function, " ++ (showPosition pos))
                    Just typ''' -> do
                        if typ''' == typ' then
                            return (env', isLoop)
                        else throwError ("Wrong return type, " ++ (showPosition pos))
    
    checkTopDef (VarDef pos typ name) = do
        (env, isLoop) <- ask
        let typ' = toMyType typ
        if typ' == MyVoid then
            throwError ("Variable of type void cannot be declared, " ++ (showPosition pos))
        else
            return (Map.insert name typ' env, isLoop)
    
    checkTopDef (VarDefAss pos typ name expr) = do
        (env, isLoop) <- ask
        let typ' = toMyType typ
        exprType <- getExprType expr
        if typ' == exprType then
            return (Map.insert name typ' env, isLoop)
        else
            throwError ("Wrong type" ++ (showPosition pos))
    
    checkBlock :: Block -> TypeCheckerMonad (Env, Maybe MyType)
    checkBlock (Blk _ stmts) =
        go stmts
        where
            go :: [Stmt] -> TypeCheckerMonad (Env, Maybe MyType)
            go [] = do
                env <- ask
                return (env, Nothing)
            go (x:xs) = do
                (env, typ) <- checkStmt x
                if isNothing typ then
                    local (const env) (go xs)
                else
                    return (env, typ)
    
    checkStmt :: Stmt -> TypeCheckerMonad (Env, Maybe MyType)
    
    checkStmt (Empty _) = do
        env <- ask
        return (env, Nothing)
    
    checkStmt (BStmt _ block) = do
        env <- ask
        (env', ret) <- checkBlock block
        return (env, ret)
    
    checkStmt (DStmt _ topDef) = do
        env <- checkTopDef topDef
        return (env, Nothing)
    
    checkStmt (Ass pos (Ident name) expr) = do
        (env, isLoop) <- ask
        case Map.lookup (Ident name) env of
            Nothing -> throwError (name ++ " is not defined, " ++ (showPosition pos))
            Just typ -> do
                typ' <- getExprType expr
                if typ /= typ' then throwError ("Expresion of a wrong type, " ++ (showPosition pos))
                else return ((env, isLoop), Nothing)
    
    checkStmt (Ret _ expr) = do
        env <- ask
        typ <- getExprType expr
        return (env, Just typ)
    
    checkStmt (VRet _) = do
        env <- ask
        return (env, Just MyVoid)
    
    checkStmt (Cond pos expr block) = do
        env <- ask
        typ <- getExprType expr
        if typ /= MyBool then
            throwError ("Expression must be of type Bool, " ++ (showPosition pos))
        else do
            (env', typ') <- checkBlock block
            return (env', Nothing)
    
    checkStmt (CondElse pos expr block1 block2) = do
        env <- ask
        typ <- getExprType expr
        if typ /= MyBool then
            throwError ("Expression must be of type Bool, " ++ (showPosition pos))
        else do
            (env1, typ1) <- checkBlock block1
            (env2, typ2) <- checkBlock block2
            case (typ1, typ2) of
                (Nothing, Nothing) -> return (env, Nothing)
                (Just a, Just b) -> do
                    if a /= b then
                        throwError ("Types do not match, " ++ (showPosition pos))
                    else
                        return (env, typ1)
                (_, _) -> throwError ("Types do not match, " ++ (showPosition pos))
    
    checkStmt (While pos expr block) = do
        typ <- getExprType expr
        if typ /= MyBool then
            throwError ("Expression must be of type Bool, " ++ (showPosition pos))
        else do
            (env, isLoop) <- ask
            ((env', isLoop'), _) <- local (const (env, True)) (checkBlock block)
            return ((env', isLoop), Nothing)
    
    checkStmt (SExp pos expr) = do
        env <- ask
        getExprType expr
        return (env, Nothing)
    
    checkStmt (Break pos) = do
        (env, isLoop) <- ask
        if not isLoop then
            throwError ("Break outside of a loop, " ++ (showPosition pos))
        else
            return ((env, isLoop), Nothing)
    
    checkStmt (Continue pos) = do
        (env, isLoop) <- ask
        if not isLoop then
            throwError ("Continue outside of a loop, " ++ (showPosition pos))
        else
            return ((env, isLoop), Nothing)
    
    checkStmt (Print pos expr) = do
        typ <- getExprType expr
        if elem typ [MyInt, MyStr, MyBool] then do
            env <- ask
            return (env, Nothing)
        else
            throwError ("Expression is not printable, " ++ (showPosition pos))
    
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
        (env, isLoop) <- ask
        case Map.lookup (Ident name) env of
            Just typ -> return typ
            Nothing -> throwError (name ++ " is not defined, " ++ (showPosition pos))
