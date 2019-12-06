module Main where

import Data.Array.IArray (elems)
import Data.Array.ST
import Data.Array.Unboxed
import Data.Foldable (toList)
import Data.List (findIndex, (!!))
import Data.List.Split (splitOn)
import Data.STRef (newSTRef, readSTRef, writeSTRef)
import Control.Monad (join)
import Control.Monad.ST (runST)
import Control.Monad.Loops (whileM_)
import qualified GHC.IO.Handle.FD as FD (openFile)
import System.Environment (getArgs)
import System.IO (IOMode(ReadMode), hGetContents)


main :: IO ()
main = do
    fPath  <- head <$> getArgs
    handle <- FD.openFile fPath ReadMode
    raw    <-  map (read :: String -> Int) . splitOn "," <$> hGetContents handle
    let len = length raw
    putStr "[1/2]: "
    print . head . compute len $ prime raw 12 2
    putStr "[2/2]: "
    print . computeNV 19690720 len $ raw


prime :: [Int] -> Int -> Int -> [Int]
prime (x:_:_:xs) l r = x:l:r:xs


compute :: Int -> [Int] -> [Int]
compute n xs =  elems $ runSTUArray $ do
    arr <- newListArray (0, n) xs
    writeArray arr n 0
    whileM_ (do
        op <- readArray arr =<< readArray arr n
        return (op /= 99)
        ) $ do
            i  <- readArray arr n
            op <- readArray arr i
            l  <- readArray arr =<< readArray arr (i + 1)
            r  <- readArray arr =<< readArray arr (i + 2)
            t  <- readArray arr (i + 3)
            writeArray arr n (i + 4)
            case op of
                1 -> writeArray arr t (l + r)
                2 -> writeArray arr t (l * r)
                _ -> error "invalid op code"
    return arr


computeNV :: Int -> Int -> [Int] -> (Int, Int)
computeNV t n xs = let
    range = (,) <$> [0..99] <*> [0..99]
    reslt = compute n . uncurry (prime xs) <$> range
    in case findIndex ((==) t . head) reslt of
        (Just x) -> range !! x
        Nothing  -> error "invalid output"
