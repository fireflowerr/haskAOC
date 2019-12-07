{-# LANGUAGE TupleSections #-}

module Main where
import Control.Monad (join)
import Data.Foldable (foldl')
import Data.List (minimumBy)
import Data.List.Split (splitOn)
import Data.Tuple (swap)
import qualified GHC.IO.Handle.FD as FD (openFile)
import System.Environment (getArgs)
import System.IO (IOMode(ReadMode), hGetContents)

main :: IO ()
main = do
    fPath  <- head <$> getArgs
    handle <- FD.openFile fPath ReadMode
    (i1:i2:_)  <- map (map format . splitOn ",") .lines <$> hGetContents handle
    let t1 = trace i1
    let t2 = trace i2
    let s1 = segments t1
    let s2 = segments t2
    let xsect = intersect s1 s2
    case nearest xsect of
        Nothing  -> putStrLn $ "[1/2] " ++ show (Nothing :: Maybe Point)
        (Just x) -> putStrLn $ "[1/2] " ++ show x ++ " - manhattan dist: " ++ show (mDist x)

type Point = (Integer, Integer)
type Path  = [Point]
type Line  = (Point, Point)

data Strategy = Horz | Vert | XSect

format :: String -> Point
format ('R':xs) = (read xs, 0)
format ('L':xs) = (negate . read $ xs, 0)
format ('U':xs) = (0, read xs)
format ('D':xs) = (0, negate . read $ xs)

mDist :: Point -> Integer
mDist (x, y) =  abs x + abs y

walk :: Point -> Point -> Point -- use scan!
walk (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

trace :: [Point] -> [Point]
trace = scanl walk (0, 0)

vert :: Line -> Bool
vert ((x1,_), (x2,_)) = x1 == x2

contains :: Integer -> Integer -> Integer -> Bool
contains a b x
        | a > b = contains b a x
        | a <= x && x <= b = True
        | otherwise = False

allEqual :: (Eq a) => [a] -> Bool
allEqual [] = True
allEqual [x] = True
allEqual (a:b:xs) = if a == b
    then allEqual $ b:xs
    else False

segments :: [Point] -> [Line]
segments [] = []
segments [_] = []
segments (x:xs) = snd . foldl' f (x, []) $ xs  where
    f (prev, acc) x = (x, (prev, x):acc)

hOverlap :: Line -> Line -> [Point]
hOverlap ((x1, y1), (x2, y2)) ((i1, y3), (i2,y4)) = let
    key1 = [(x1, x2, [i1, i2]), (i1, i2, [x1, x2])]
    f :: (Integer, Integer, [Integer]) -> [Point]
    f (a, b, e) = (, y1) <$> filter (contains a b) e
    in f =<< key1

vOverlap :: Line -> Line -> [Point]
vOverlap (a1, a2) (b1, b2) = swap <$> hOverlap a' b' where
    a' = (swap a1, swap a2)
    b' = (swap b1, swap b2)

xOverlap :: Line -> Line -> Maybe Point
xOverlap a b = let
    rH = hOverlap a b
    rV = vOverlap a b
    in case [rH, rV] of
        [(x:_), (y:_)] -> Just (fst x, snd y)
        _          -> Nothing

nearest :: [Point] -> Maybe Point
nearest [] = Nothing
nearest xs = Just . minimumBy f $ xs where
    f a b = mDist a `compare` mDist b

select :: Line -> Line -> Strategy
select a b = case (vert a, vert b) of
    (False, False) -> Horz
    (True, True)   -> Vert
    _              -> XSect

control :: Strategy -> Line -> Line -> [Point]
control s a b = f s where
    g Nothing  = []
    g (Just x) = [x]
    fixHorz ((_,y1),(_,y2)) ((_,y3),(_,y4)) = if allEqual [y1, y2, y3, y4]
        then Just (a, b)
        else Nothing
    fixVert ((x1,_),(x2,_)) ((x3,_),(x4,_)) = if allEqual [x1, x2, x3, x4]
        then Just (a, b)
        else Nothing
    f Horz  = uncurry hOverlap =<< g (fixHorz a b)
    f Vert  = uncurry vOverlap =<< g (fixVert a b)
    f XSect = g $ a `xOverlap` b

intersect :: [Line] -> [Line] -> [Point]
intersect [] _ = []
intersect _ [] = []
intersect a b  = filter (/= (0,0)) . join $ (\x y -> (control (select x y) x y) ) <$> a <*> b
