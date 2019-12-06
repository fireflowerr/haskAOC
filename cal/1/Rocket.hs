module Main where
import qualified GHC.IO.Handle.FD as FD (openFile)
import System.IO (IOMode(ReadMode), hGetContents)
import System.Environment (getArgs)


main :: IO ()
main = do
    fPath  <- head <$> getArgs
    handle <- FD.openFile fPath ReadMode
    input  <- filter (not . null) . lines <$> hGetContents handle
    putStrLn $ "fuel : " ++ show (sum . map (fuel  . read :: String -> Integer) $ input)
    putStrLn $ "fuel': " ++ show (sum . map (fuel' . read :: String -> Integer) $ input)


fuel :: Integer -> Integer
fuel = subtract 2 . (`div` 3)


fuel' :: Integer -> Integer
fuel' = f . fuel where
    f x
        | x > 0 = x + (f . fuel $ x)
        | otherwise = 0
