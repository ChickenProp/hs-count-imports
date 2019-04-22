module Main
    ( main
    )
where

import           System.Exit                    ( exitFailure )
import           System.IO                      ( readFile )

import           A                              ( (^&)
                                                , foo
                                                )

-- Our testing strategy is: assume that this file was compiled with the correct
-- flags, and that we're running in the root project directory. Then just check
-- that the file produced in compilation is exactly what we expect.

myImports :: [(String, String)]
myImports =
    [ ("System.Exit", "exitFailure")
    , ("System.IO"  , "readFile")
    , ("A"          , "(^&)")
    , ("A"          , "foo")
    ]

main = do
    let expected = unlines $ map (\(a, b) -> a ++ "\t" ++ b) myImports
    actual <- readFile "test/test-out/Mainmain"
    if actual == expected
        then putStrLn "OK" >> return ()
        else do
            putStrLn "Expected:"
            putStrLn expected
            putStrLn "\n---\n"
            putStrLn "Actual:"
            putStrLn actual
            exitFailure
