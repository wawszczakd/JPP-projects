module Interpreter where
    import ParLagerLang
    import AbsLagerLang
    import TypeChecker
    import Evaluator
    import Control.Monad.Except
    import Control.Monad.Reader
    import Control.Monad.State
    import System.Exit
    import Data.Map as Map

    interpretFile :: FilePath -> IO ()
    interpretFile file = readFile file >>= interpret
    
    interpretStdin :: IO ()
    interpretStdin = do
        getContents >>= interpret
    
    interpret :: String -> IO ()
    interpret input = run $ pProgram $ myLexer input

    run :: Either String Program -> IO ()
    run (Left err) = do
        print err
        exitFailure
    run (Right tree) = do
        res <- runExceptT $ runReaderT (checkProgram tree) (Map.empty, False)
        case res of
            Right () -> do
                putStrLn "Type checking successful"
                res' <-  runExceptT $ runStateT (runReaderT (evalProgram tree) Map.empty) (Map.empty, 0)
                case res' of
                    Right ((), _) -> putStrLn "Runtime finished"
                    Left err -> putStrLn err
            Left err -> putStrLn $ "Type checking failed: " ++ err
