module Evaluator where
    import AbsLagerLang
    import Common
    import Control.Monad.Reader
    import Control.Monad.State
    import Control.Monad.Except
    import Data.Map as Map
    import Data.Maybe
    
    type Loc = Integer
    type Env = Map.Map Ident Loc
    data MyVal = MyInt Integer | MyStr String | MyBool Bool | MyVoid | MyFun [Arg] Block Env
    type Store = (Map.Map Loc MyVal, Loc)
    
    type EvaluatorMonad = ReaderT Env (StateT Store (ExceptT String IO))
    
    evalProgram :: Program -> EvaluatorMonad ()
    evalProgram (Prog _ xs) =
        go xs
        where
            go :: [TopDef] -> EvaluatorMonad ()
            go [] = do
                env <- ask
                local (const env) (evalExpr (EApp Nothing (Ident "main") []))
                return ()
            go (x:xs) = do
                env <- evalTopDef x
                local (const env) (go xs)
    
    evalTopDef :: TopDef -> EvaluatorMonad Env
    
    evalTopDef (FnDef _ _ name args block) = do
        env <- ask
        (store, lastLoc) <- get
        let env' = Map.insert name lastLoc env
        put (Map.insert lastLoc (MyFun args block env') store, lastLoc + 1)
        return env'
    
    evalTopDef (VarDef _ typ name) = do
        env <- ask
        (store, lastLoc) <- get
        let env' = Map.insert name lastLoc env
            store' = case typ of
                Int _ -> (Map.insert lastLoc (MyInt 0) store, lastLoc + 1)
                Str _ -> (Map.insert lastLoc (MyStr "") store, lastLoc + 1)
                Bool _ -> (Map.insert lastLoc (MyBool False) store, lastLoc + 1)
        put store'
        return env'
    
    evalTopDef (VarDefAss _ typ name expr) = do
        env <- ask
        (store, lastLoc) <- get
        val <- evalExpr expr
        let env' = Map.insert name lastLoc env
        put (Map.insert lastLoc val store, lastLoc + 1)
        return env'
    
    evalBlock :: Block -> EvaluatorMonad (Env, Maybe MyVal)
    evalBlock (Blk _ stmts) = do
        go stmts
        where
            go :: [Stmt] -> EvaluatorMonad (Env, Maybe MyVal)
            go [] = do
                env <- ask
                return (env, Nothing)
            go (x:xs) = do
                (env, ret) <- evalStmt x
                if isNothing ret then
                    local (const env) (go xs)
                else
                    return (env, ret)
    
    evalStmt :: Stmt -> EvaluatorMonad (Env, Maybe MyVal)
    
    evalStmt (Empty _) = do
        env <- ask
        return (env, Nothing)
    
    evalStmt (BStmt _ block) = do
        env <- ask
        (env', ret) <- evalBlock block
        return (env, ret)
    
    evalStmt (DStmt _ topDef) = do
        env <- evalTopDef topDef
        return (env, Nothing)
    
    evalStmt (Ass _ name expr) = do
        env <- ask
        val <- evalExpr expr
        (store, lastLoc) <- get
        let Just loc = Map.lookup name env
        put (Map.insert loc val store, lastLoc)
        return (env, Nothing)
    
    evalStmt (Ret _ expr) = do
        env <- ask
        val <- evalExpr expr
        return (env, Just val)
    
    evalStmt (VRet _) = do
        env <- ask
        return (env, Just MyVoid)
    
    evalStmt (Cond _ expr block) = do
        env <- ask
        MyBool val <- evalExpr expr
        if val then
            evalBlock block
        else
            return (env, Nothing)
    
    evalStmt (CondElse _ expr block1 block2) = do
        env <- ask
        MyBool val <- evalExpr expr
        if val then
            evalBlock block1
        else
            evalBlock block2
    
    evalStmt (While _ expr block) = do
        MyBool val <- evalExpr expr
        if val then do
            (env, ret) <- evalBlock block
            if isNothing ret then
                evalStmt (While Nothing expr block)
            else
                return (env, ret)
        else do
            env <- ask
            return (env, Nothing)
    
    evalStmt (SExp _ expr) = do
        env <- ask
        val <- evalExpr expr
        return (env, Nothing)
    
    evalStmt (Break _) = do
        env <- ask
        return (env, Nothing)
    
    evalStmt (Continue _) = do
        env <- ask
        return (env, Nothing)
    
    evalStmt (Print _ expr) = do
        tmp <- evalExpr expr
        let showable = case tmp of
                MyInt val -> show val
                MyStr val -> val
                MyBool True -> "true"
                MyBool False -> "false"
        liftIO $ putStrLn showable
        env <- ask
        return (env, Nothing)
    
    insertArgs :: [Arg] -> [Expr] -> Env -> EvaluatorMonad Env
    insertArgs [] [] _ = ask
    insertArgs ((ValArg _ _ name):args) (expr:exprs) env = do
        funEnv <- insertArgs args exprs env
        val <- local (const env) (evalExpr expr)
        (store, lastLoc) <- get
        let funEnv' = Map.insert name lastLoc funEnv
        put (Map.insert lastLoc val store, lastLoc + 1)
        return funEnv'
    insertArgs ((RefArg _ _ name):args) ((EVar _ origName):exprs) env = do
        funEnv <- insertArgs args exprs env
        let Just loc = Map.lookup origName env
            funEnv' = Map.insert name loc funEnv
        return funEnv'
    
    evalExpr :: Expr -> EvaluatorMonad MyVal
    
    evalExpr (EVar _ name) = do
        env <- ask
        (store, _) <- get
        let Just loc = Map.lookup name env
            Just val = Map.lookup loc store
        return val
    
    evalExpr (ELitInt _ val) = return $ MyInt val
    
    evalExpr (ELitTrue _) = return $ MyBool True
    
    evalExpr (ELitFalse _) = return $ MyBool False
    
    evalExpr (EApp _ name exprs) = do
        env <- ask
        (store, _) <- get
        let Just loc = Map.lookup name env
            Just (MyFun args block funEnv) = Map.lookup loc store
        funEnv' <- local (const funEnv) (insertArgs args exprs env)
        (funEnv'', ret) <- local (const funEnv') (evalBlock block)
        case ret of
            Nothing -> return MyVoid
            Just ret' -> return ret'
    
    evalExpr (EString _ s) = return $ MyStr s
    
    evalExpr (Neg _ expr) = do
        MyInt val <- evalExpr expr
        return $ MyInt (-val)
    
    evalExpr (Not _ expr) = do
        MyBool val <- evalExpr expr
        case val of
            True -> return $ MyBool False
            _ -> return $ MyBool True
    
    evalExpr (EMul pos expr1 op expr2) = do
        MyInt val1 <- evalExpr expr1
        MyInt val2 <- evalExpr expr2
        case op of
            Times _ -> return $ MyInt (val1 * val2)
            Div _ ->
                if val2 == 0 then
                    throwError ("Runtime error: division by 0, " ++ (showPosition pos))
                else
                    return $ MyInt (val1 * val2)
            Mod _ ->
                if val2 == 0 then
                    throwError ("Runtime error: division by 0, " ++ (showPosition pos))
                else
                    return $ MyInt (mod val1 val2)
    
    evalExpr (EAdd _ expr1 op expr2) = do
        MyInt val1 <- evalExpr expr1
        MyInt val2 <- evalExpr expr2
        case op of
            Plus _ -> return $ MyInt (val1 + val2)
            Minus _ -> return $ MyInt (val1 - val2)
    
    evalExpr (ERel _ expr1 op expr2) = do
        MyInt val1 <- evalExpr expr1
        MyInt val2 <- evalExpr expr2
        case op of
            LTH _  -> return $ MyBool (val1 < val2)
            LE _   -> return $ MyBool (val1 <= val2)
            GTH _  -> return $ MyBool (val1 > val2)
            GE _   -> return $ MyBool (val1 >= val2)
            EQU _  -> return $ MyBool (val1 == val2)
            NE _   -> return $ MyBool (val1 /= val2)
    
    evalExpr (EAnd _ expr1 expr2) = do
        MyBool val1 <- evalExpr expr1
        MyBool val2 <- evalExpr expr2
        case (val1, val2) of
            (True, True) -> return $ MyBool True
            _ -> return $ MyBool False
    
    evalExpr (EOr _ expr1 expr2) = do
        MyBool val1 <- evalExpr expr1
        MyBool val2 <- evalExpr expr2
        case (val1, val2) of
            (False, False) -> return $ MyBool False
            _ -> return $ MyBool True
