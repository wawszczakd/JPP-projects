module Main where
    import Interpreter
    import System.Environment
    import System.Exit
    
    main :: IO ()
    main = do
        args <- getArgs
        case args of
            ["--help"] -> do
                putStrLn $ unlines
                    [ "usage: Call with one of the following argument combinations:"
                    , "  --help          Display this help message."
                    , "  (no arguments)  Parse standard input."
                    , "  <file>          Parse content of the file." ]
                exitFailure
            [f]        -> interpretFile f
            []         -> interpretStdin
            _          -> do
                putStrLn "Invalid usage! Use --help."
                exitFailure
