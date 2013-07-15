module Main where

import           Text.ParserCombinators.Parsec
import Data.List (sortBy)

parseInput =
    do dirs <- many dirAndSize
       eof :: Parser ()
       return dirs

data Dir = Dir {dir_size::Int, dir_name::String} deriving Show

data DirPack = DirPack {pack_size::Int, dirs::[Dir]} deriving Show

media_size = 700*1024*1024

dirAndSize =
    do size <- many1 digit
       spaces
       dir_name <- anyChar `manyTill` newline
       return (Dir (read size) dir_name)

greedy_pack dirs = foldl maybe_add_dir (DirPack 0 []) $ sortBy cmpSize dirs
    where
    cmpSize d1 d2 = compare (dir_size d1) (dir_size d2)

maybe_add_dir p d = 
    let new_size = pack_size p + dir_size d
        new_dirs = d:(dirs p)
        in if new_size > media_size then p else DirPack new_size new_dirs

main :: IO()
main = do input <- getContents
          putStrLn ("DEBUG: got input " ++ input)
          let dirs = case parse parseInput "stdin" input of
                     Left err -> error $ "Input:\n" ++ show input ++
                                         "\nError:\n" ++ show err
                     Right result -> result
          putStrLn "DEBUG: parsed:"; print dirs
          putStrLn "Solution:"; print (greedy_pack dirs)
