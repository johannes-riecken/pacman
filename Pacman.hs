-- {-# OPTIONS_GHC -Wall #-}
-- {-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE ViewPatterns, ScopedTypeVariables #-}

import Numeric.Natural
import Linear hiding (E)
import Safe
import Data.Maybe
import Data.Function.Pointless
import Data.List
import Test.QuickCheck
import Data.List.Extra (snoc)
import Control.Monad
import Test.QuickCheck.Instances.Natural
import GHC.Real
import Foreign.C
import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal.Array
import Foreign.Marshal.Alloc
import System.IO.Unsafe
import Data.Vector.Storable (Vector(..))

foreign import ccall
    doubleArray :: Ptr CInt -> CSize -> Ptr CSize -> IO (Ptr CInt)

foreign import ccall
    neighborsImpl :: CInt -> CInt -> CInt -> CInt -> Ptr (V2 CInt)

neighbors :: CInt -> CInt -> CInt -> CInt -> [(CInt,CInt)]
neighbors x y dir_x dir_y =
    fmap v2ToTuple .
        unsafePerformIO .
        peekArray 8 $ -- convert int[8] to haskell list
        neighborsImpl x y dir_x dir_y

substrings :: Natural -> [a] -> [[a]]
substrings n xs = catMaybes $ fmap (takeMay n) (tails xs)

prop_substringsSizeN :: Natural -> [Int] -> Bool
prop_substringsSizeN n xs = all ((==n) . genericLength) (substrings n xs)

prop_substringsNumber :: Natural -> [Int] -> Property
prop_substringsNumber n xs =
    genericLength xs >= n && n > 0 ==>
        genericLength xs - n + 1 === genericLength (substrings n xs)

unsubstrings :: [[a]] -> [a]
unsubstrings [] = []
unsubstrings ([]:_) = []
unsubstrings xs = fmap head (init xs) <> last xs

prop_substringsIso :: Natural -> [Int] -> Property
prop_substringsIso n xs =
    genericLength xs >= n && n > 0 ==>
        unsubstrings (substrings n xs) === xs

takeMay :: Natural -> [a] -> Maybe [a]
takeMay n xs | n <= genericLength xs = Just $ genericTake n xs
takeMay _ _ = Nothing

prop_neighborNeighborSelf :: CInt -> CInt -> CInt -> CInt -> Bool
prop_neighborNeighborSelf x y dir_x dir_y =
    all ((x,y) `elem`) $
        fmap
            (\(x',y') -> neighbors x' y' (negate dir_x) (negate dir_y))
            (neighbors x y dir_x dir_y)

prop_neighborNotSelf :: CInt -> CInt -> CInt -> CInt -> Bool
prop_neighborNotSelf x y dir_x dir_y =
    (x,y) `notElem` neighbors x y dir_x dir_y

foreign import ccall "filterCorners" filterCornersImpl :: Ptr (V2 CInt) -> CSize -> Ptr CSize -> IO (Ptr (V2 CInt))

filterCorners :: [V2 CInt] -> [V2 CInt]
filterCorners xs = unsafePerformIO $ do
  arr <- newArray xs
  p_outlen :: Ptr CSize <- malloc
  ret <- filterCornersImpl arr (genericLength xs) p_outlen
  outlen <- peek p_outlen
  peekArray (fromIntegral outlen) ret

prop_onlyCorners :: [V2 CInt] -> Bool
prop_onlyCorners xs =
    and . fmap
        (\[x,y] ->
            normalize (fmap fromIntegral x :: V2 Float) /=
            normalize (fmap fromIntegral y)) .
        substrings 2 .
        fromMaybe [] $
        zipWith (^-^) <$> tailMay xs <*> Just xs

prop_lerpCornersId :: [V2 CInt] -> Property
prop_lerpCornersId xs = lerp' (filterCorners xs) === xs

prop_findsCorners :: [V2 CInt] -> Property
prop_findsCorners xs =
    length xs > 0 ==>
    length (filterCorners xs) > 0

prop_randWalkUnlerpable :: RandomWalk -> Property
prop_randWalkUnlerpable (RandomWalk xs) =
    lerp' (fmap (\(x,y)->V2 x y) xs) === (fmap (\(x,y)->V2 x y) xs)

zipWithLub :: (a -> a -> [a]) -> [a] -> [a] -> [[a]]
zipWithLub _ xs [] = [xs]
zipWithLub _ [] ys = [ys]
zipWithLub f xs ys = zipWith f xs ys

zipWithLub' :: (a -> a -> [a]) -> [a] -> [a] -> [a]
zipWithLub' _ xs [] = xs
zipWithLub' _ [] ys = ys
zipWithLub' f xs ys = fromJust $ (<>) <$> headMay res <*> (join . fmap (drop 1) <$> tailMay res) where res = zipWith f xs ys

lerp' :: [V2 CInt] -> [V2 CInt]
-- lerp' xs = join $ zipWithLub lerpLine xs (tail xs)
lerp' (fmap v2ToTuple -> xs) = fmap tupleToV2 $ zipWithLub' lerpLine' xs (tail xs)

lerpLine' :: (CInt,CInt) -> (CInt,CInt) -> [(CInt,CInt)]
lerpLine' (x0,y0) (x1,y1) =
    zip (if x1 - x0 == 0 then
        genericReplicate (abs (y1 - y0) + 1) x0
    else
        enumFromThenTo x0 (x0 + signum (x1 - x0)) x1)
    (if y1 - y0 == 0 then
         genericReplicate (abs (x1 - x0) + 1) y0
     else
         enumFromThenTo y0 (y0 + signum (y1 - y0)) y1)

prop_lerpLineAdjPoints :: CInt -> CInt -> (CInt,CInt) -> Property
prop_lerpLineAdjPoints x y (a,b) =
    lerpLine' (a,b) (a+signum x,b+signum y) === [(a,b),(a+signum x,b+signum y)]

prop_lerpIsSupersequence :: (CInt,CInt) -> (CInt,CInt) -> Bool
prop_lerpIsSupersequence a b = [a,b] `isSubsequenceOf` lerpLine' a b

tupleToV2 :: (CInt,CInt) -> V2 CInt
tupleToV2 = (\(x,y) -> V2 x y)

v2ToTuple :: V2 CInt -> (CInt,CInt)
v2ToTuple = (\(V2 x y) -> (x,y))

lerpLine :: V2 CInt -> V2 CInt -> [V2 CInt]
lerpLine p0@(V2 x0 y0) p1@(V2 x1 y1) =
    fmap (\n -> (p0 ^+^) $
        floor <$> lerp (fromIntegral n / fromIntegral (max (abs $ x1 - x0) (abs $ y1 - y0))) (fmap fromIntegral p1) (fmap fromIntegral p0)) [0..max (abs $ x1 - x0) (abs $ y1 - y0)] 

newtype RandomWalk = RandomWalk [(CInt,CInt)] deriving (Show)

instance Arbitrary RandomWalk where
  arbitrary =
    fmap (RandomWalk .
          scanl (\(a,b) (c,d)->(a+c,b+d)) (0,0) .
          filter (/=(0,0)))
              (listOf $ (,) <$> elements [-1..1] <*> elements [-1..1])

-- foreign import ccall "walkLine" walkLineImpl :: Img -> CInt -> CInt -> Seen -> [Point]

main = do
  putStrLn "Beginning tests"
  -- quickCheck $ \(RandomWalk xs) -> prop_onlyCorners (filterCorners . fmap (\(x,y) -> V2 x y) $ xs)
  -- quickCheck $ prop_lerpIsSupersequence
  -- quickCheck $ \x y z->prop_lerpLineAdjPoints x y z
  quickCheck $ \(RandomWalk xs) -> prop_lerpCornersId (fmap (\(x,y)->V2 x y) xs)
  -- quickCheck $ \(RandomWalk xs) -> prop_findsCorners (fmap (\(x,y) -> V2 x y) xs)
  -- n <- neighbors 0 0 1 1
  -- print n
  -- m <- neighbors 1 1 1 1
  -- print m
  -- quickCheck $ prop_neighborNeighborSelf
  -- quickCheck $ prop_neighborNotSelf
  -- quickCheck $ \(x0,y0) (x1,y1) -> fmap (uncurry V2) (lerpLine' (x0,y0) (x1,y1)) === lerpLine (V2 x0 y0) (V2 x1 y1)
  -- quickCheck prop_substringsSizeN
  -- quickCheck prop_substringsIso
  -- quickCheck prop_substringsNumber
-- findLines = walk
-- 
-- walk = if black || seen then walkOn else walkLine fromLeft
-- 
-- walkLine fromWhere = if neighborDirectionExcluding fromWhere != opposite fromWhere then put pixel else pure ()
-- 
width :: Int
-- width = 224
width = length (head img)

height :: Int
-- height = 288
height = length img

img :: [[Int]]
img = [[0,0,1,1,1,0,0]]

indices :: [(Int,Int)]
indices = (,) <$> [0..width - 1] <*> [0..height - 1]

-- runImage :: [[(Int,Int)]]
-- runImage = traverse (\idx -> do 
--   (dir,seen) <- get
--   guard (img !! idx == 1 && idx `notElem` seen)
--   let (dir,neighbor) = head $ filter (\(dir,idx) -> idx `notElem` seen && img !! idx == 1) (neighbors idx)
--   innerTraverse (dir,seen)) indices
-- 
innerTraverse (dir,seen) = 5

data Dir = E | NE | N | NW | W | SW | S |SE deriving (Show,Enum,Bounded)

-- dirToNeighbor :: (Int,Int) -> Dir -> (Dir,(Int,Int))
-- dirToNeighbor point dir = neighbors point !! fromEnum dir
-- 
-- neighbors :: (Int,Int) -> [(Dir,(Int,Int))]
-- neighbors (x,y) = [
--   (E ,(x+1,y  )),
--   (NE,(x+1,y+1)),
--   (N ,(x  ,y+1)),
--   (NW,(x-1,y+1)),
--   (W ,(x-1,y  )),
--   (SW,(x-1,y-1)),
--   (S ,(x  ,y-1)),
--   (SE,(x+1,y-1))
--   ]

-- state is current direction, seen white pixels,
-- write-only state is polyline points
-- outer traverse loops over indices
-- inner "traverse" combines endless repetition of state functions with kleisli arrows
