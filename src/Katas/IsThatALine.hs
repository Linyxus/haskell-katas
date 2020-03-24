-- | Kata: Line Safari - 3 kyu
module Katas.IsThatALine where

import Data.Set (Set)
import qualified Data.Set as S
import Data.Map (Map)
import qualified Data.Map as M
import Debug.Trace

type Pos = (Int, Int)
type Maze = Map Pos Char
type Vis = Set Pos
data Path = Path { path :: [Pos], vis :: Vis }deriving (Eq, Show)

conPath :: Pos -> Path -> Path
conPath ch (Path p v) = Path (ch:p) (S.insert ch v)

expandPath :: Maze -> Path -> [Path]
expandPath m path@(Path (pos:_) vis)
  = fmap g . filter f . filter notVis . filterNull $ around pos
  where filterNull :: [(Dir, Pos)] -> [(Dir, Pos, Char)]
        filterNull = go . fmap (\(d, v) -> (d, v, M.lookup v m))
          where go ((_, _, Nothing):xs) = go xs
                go ((d, p, (Just c)):xs) = (d, p, c) : go xs
                go [] = []
        notVis :: (Dir, Pos, Char) -> Bool
        notVis (_, p, _) = p `S.notMember` vis
        f :: (Dir, Pos, Char) -> Bool
        f (d, _, c) = case fromMaybe (M.lookup pos m) of
              '+' -> cornerP d c
              '|' -> vertP d c
              '-' -> horiP d c
              'X' -> xP d c
        g :: (Dir, Pos, Char) -> Path
        g = flip conPath path . \(x,y,z) -> y

expandN :: Maze -> Int -> [Path] -> [Path]
expandN m 0 = id
expandN m n = (>>= expandPath m) . expandN m (n-1)

locateSrc :: [String] -> Pos
locateSrc = go 0
  where go :: Int -> [String] -> Pos
        go _ [] = error "Not found!"
        go n (x:xs) = case f 0 x of
                        Nothing -> go (n+1) xs
                        Just m -> (n, m)
        f _ [] = Nothing
        f m ('X':_) = Just m
        f m (_:xs) = f (m+1) xs

mkMaze :: [String] -> Maze
mkMaze = M.fromList . filter notSpace . mconcat . zipWith pairLine [0..] . fmap pairCol
  where pairCol :: String -> [(Int, Char)]
        pairCol = zip [0..]
        pairLine :: Int -> [(Int, Char)] -> [((Int, Int), Char)]
        pairLine n = fmap $ \(i, c) -> ((n, i), c)
        notSpace :: ((Int, Int), Char) -> Bool
        notSpace (_, c) = c /= ' '

mkInit :: [String] -> Path
mkInit src = Path [p] $ S.fromList [p]
  where p = locateSrc src

-- >>> src = ["X-|X"]
-- >>> m = mkMaze src
-- >>> m
-- fromList [((0,0),'X'),((0,1),'-'),((0,2),'|'),((0,3),'X')]
-- >>> init = mkInit src
-- >>> expandN m 1 [init]
-- [Path {path = [(0,1),(0,0)], vis = fromList [(0,0),(0,1)]}]

cornerP :: Dir -> Char -> Bool
cornerP _ 'X' = False
cornerP U '|' = True
cornerP D '|' = True
cornerP L '-' = True
cornerP R '-' = True
cornerP _ _ = False

vertP :: Dir -> Char -> Bool
vertP _ '-' = False
vertP d _ = d == U || d == D

horiP :: Dir -> Char -> Bool
horiP _ '|' = False
horiP d _ = d == L || d == R

xP :: Dir -> Char -> Bool
xP _ 'X' = True
xP U '|' = True
xP D '|' = True
xP L '-' = True
xP R '-' = True
xP _ _ = False

posAdd :: Pos -> Pos -> Pos
posAdd (x, y) (u, v) = (x + u, y + v)

data Dir = L | R | U | D deriving (Eq, Show)

around :: Pos -> [(Dir, Pos)]
around p = zip [U, D, L, R] $ map (posAdd p) [(-1, 0), (1, 0), (0, -1), (0, 1)]

fromMaybe :: Maybe a -> a
fromMaybe (Just x) = x

end :: Maze -> Path -> Bool
end m (Path (p:_) _) = fromMaybe (M.lookup p m) == 'X'

reachable :: Maze -> [Path] -> Bool
reachable _ [] = False
reachable m ps
  | any (end m) ps = True
  | otherwise = reachable m $ ps >>= expandPath m

line :: [String] -> Bool
line src = trace (show src) $ reachable maze $ expandPath maze init
  where maze = mkMaze src
        init = mkInit src

src = ["      +------+","      |      |","X-----+------+","      |       ","      X       "]

showSrc :: [String] -> IO ()
showSrc = mapM_ putStrLn

-- >>> showSrc src
--       +------+
--       |      |
-- X-----+------+
--       |
--       X

-- >>> line src
-- ["      +------+","      |      |","X-----+------+","      |       ","      X       "]
-- True
