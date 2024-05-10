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
        result <- runExceptT $ runReaderT (typeCheck tree) (Map.empty, False)
        case result of
            Right () -> putStrLn "Type checking successful"
            Left err -> putStrLn $ "Type checking failed: " ++ err
