module Main where

main ::  IO ()
main = do putStrLn "Hey, what's your name?"
          name <- getLine
          putStrLn ("Hey there " ++ name ++ "!")
          putStrLn "What's your favorite color?"
          color <- getLine
          putStrLn ("Really?!? You like " ++ color ++ " that much?")
