module Main where

import           Control.Monad                 (liftM2, replicateM)
import           Data.List                     (sortBy, maximumBy)
import           Data.Ix                       (inRange)
import           Test.QuickCheck
import           Text.ParserCombinators.Parsec

parseInput =
    do dirs <- many dirAndSize
       eof :: Parser ()
       return dirs

data Dir = Dir {dir_size::Integer, dir_name::String} deriving Show
instance Eq Dir where
    (==) d1 d2 = dir_size d1 == dir_size d2 &&
                 dir_name d1 == dir_name d2

data DirPack = DirPack {pack_size::Integer, dirs::[Dir]} deriving Show

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

-- Taken from 'cd-fit-4-1.hs'
----------------------------------------------------------------------------------
-- Dynamic programming solution to the knapsack (or, rather, disk) packing problem
--
-- Let the `bestDisk x' be the "most tightly packed" disk of total
-- size no more than `x'.
precomputeDisksFor :: [Dir] -> [DirPack]
precomputeDisksFor dirs =
      -- By calculating `bestDisk' for all possible disk sizes, we could
      -- obtain a solution for particular case by simple lookup in our list of
      -- solutions :)
  let precomp = map bestDisk [0..]

      -- How to calculate `bestDisk'? Lets opt for a recursive definition:
      -- Recursion base: best packed disk of size 0 is empty
      bestDisk 0 = DirPack 0 []
      -- Recursion step: for size `limit`, bigger than 0, best packed disk is
      -- computed as follows:
      bestDisk limit =
         -- 1. Take all non-empty dirs that could possibly fit to that disk by itself.
         --    Consider them one by one. Let the size of particular dir be `dir_size d'.
         --    Let's add it to the best-packed disk of size <= (limit - dir_size d), thus
         --    producing the disk of size <= limit. Lets do that for all "candidate"
         --    dirs that are not yet on our disk:
         case [ DirPack (dir_size d + s) (d:ds)
                | d <- filter ( (inRange (1,limit)).dir_size ) dirs
                  , dir_size d > 0
                  , let (DirPack s ds)=precomp!!(fromInteger (limit - dir_size d))
                  , d `notElem` ds
              ] of
                -- We either fail to add any dirs (probably, because all of them too big).
                -- Well, just report that disk must be left empty:
                [] -> DirPack 0 []
                -- Or we produce some alternative packings. Let's choose the best of them all:
                packs -> maximumBy cmpSize packs

      cmpSize a b = compare (pack_size a) (pack_size b)

      in precomp

-- When we precomputed disk of all possible sizes for the given set of dirs, solution to
-- particular problem is simple: just take the solution for the required 'media_size' and
-- that's it!
dynamic_pack dirs = (precomputeDisksFor dirs)!!(fromInteger media_size)


main :: IO()
main = do input <- getContents
          putStrLn ("DEBUG: got input " ++ input)
          let dirs = case parse parseInput "stdin" input of
                     Left err -> error $ "Input:\n" ++ show input ++
                                         "\nError:\n" ++ show err
                     Right result -> result
          putStrLn "DEBUG: parsed:"; print dirs
          putStrLn "Solution:"; print (dynamic_pack dirs)

--Tests
instance Arbitrary Dir where
    arbitrary = liftM2 Dir gen_size gen_name
        where gen_size = do s <- choose (10,1400)
                            return (s*1024*1024)
              gen_name = do n <- choose (1,300)
                            replicateM n (elements "fubar/")

prop_greedy_pack_is_fixpoint ds =
    let pack = greedy_pack ds
        in pack_size pack == pack_size (greedy_pack (dirs pack))

prop_dynamic_pack_is_fixpoint ds =
    let pack = dynamic_pack ds
        in pack_size pack == pack_size (dynamic_pack (dirs pack))
