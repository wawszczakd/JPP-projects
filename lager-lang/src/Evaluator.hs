module Evaluator where
    import AbsLagerLang
    import Common
    import Control.Monad.Reader
    import Control.Monad.State
    import Control.Monad.Except
    import Data.Map as Map
    
    type Loc = Int
    type Env = Map.Map Ident Loc
    data MyVal = MyInt Integer | MyStr String | MyBool Bool
    type Store = (Map.Map Loc MyVal, Loc)
    
    type EvaluatorMonad = ReaderT Env (StateT Store (ExceptT String IO))
    
    evalExpr :: Expr -> EvaluatorMonad MyVal
    
    evalExpr (EVar _ name) = do
        env <- ask
        let Just loc = Map.lookup name env
        (store, _) <- get
        let Just val = Map.lookup loc store
        return val
    
    evalExpr (ELitInt _ val) = return $ MyInt val
    
    evalExpr (ELitTrue _) = return $ MyBool True
    
    evalExpr (ELitFalse _) = return $ MyBool False
    
    evalExpr (EApp _ name args) = return $ MyInt 0
    
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
        MyBool val1 <- evalExpr expr1
        MyBool val2 <- evalExpr expr2
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
