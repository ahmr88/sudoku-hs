module Solver where

import Board
import Data.List
import Data.Function (on)
import Data.Either (fromLeft, isLeft)
import qualified Data.Heap as H
import qualified Data.Vector as V


-- Testing
import Data.Maybe
import System.IO.Unsafe

type Square = Either [Int] Int

newtype Board = Board (V.Vector (V.Vector (Square))) deriving (Show, Eq)

instance Ord Board where
  compare = compare `on` emptySquares

emptySquares :: Board -> Int
emptySquares (Board b) = V.sum $ V.map (V.foldr (\e acc -> aux e + acc) 0) b
  where aux (Left _) = 1
        aux (Right _) = 0

squareToInt :: Square -> Int
squareToInt (Right a) = a
squareToInt (Left _) = 0

toBoard :: RawBoard -> Board
toBoard rb = Board $ V.fromList $ map (V.fromList . map intToSquare) rb
  where intToSquare 0 = Left [1 .. (rootSize rb)^2]
        intToSquare i = Right i

toRawBoard :: Board -> RawBoard
toRawBoard (Board b) = V.toList $ V.map (V.toList . V.map squareToInt) b

boxWise :: Int -> (Int, Int) -> (Int, Int)
boxWise sqt (i, j) = case (i, j) of
                       (i, j) 
                         | j == (boxNo j + sqt - 1) && i < (boxNo i + sqt - 1) -> (i + 1, boxNo j)
                         | j < (boxNo j + sqt - 1)                             -> (i, j + 1)
                         | otherwise                                           -> (-1, -1)
    where boxNo n = sqt * (n `div` sqt)

colWise :: Int -> (Int, Int) -> (Int, Int)
colWise sqt (i, j) 
  | i == sqt^2 - 1 || i == -1 = (-1, -1)
  | otherwise                 = (i + 1, j)

rowWise :: Int -> (Int, Int) -> (Int, Int)
rowWise sqt (i, j) 
  | j == sqt^2 - 1 || j == -1 = (-1, -1)
  | otherwise                 = (i, j + 1)

clear :: ((Int, Int) -> (Int, Int)) -> (Int, Int) -> Int -> Board -> Maybe Board
clear next (i, j) v b = func (Just b) (i, j)
  where func :: Maybe Board -> (Int, Int) -> Maybe Board
        func Nothing  _      = Nothing
        func (Just (Board b)) (i, j) = case (b V.!? i) >>= (\r -> r V.!? j) of 
                           Nothing -> Just $ Board b
                           (Just (Right _)) -> func (Just $ Board b) $ next (i, j)
                           (Just (Left s)) -> func removed $ next (i, j)
                             where removed = fillSquareWith (Board b) (i, j) $ Left $ delete v s

fillSquares :: Board -> (Int, Int) -> Maybe Board
fillSquares (Board b) (i, j) 
  | i == (rootSize b)^2 - 1 && j == (rootSize b)^2 - 1 = fillSquareWith (Board b) (i, j) 
                                                       $ possibleVals (Board b) (i, j)
  | otherwise = do
    b' <- fillSquareWith (Board b) (i, j) $ possibleVals (Board b) (i, j)
    let (i', j') = case (j + 1) `mod` (rootSize b)^2 of
                     0  -> ((i + 1) `mod` (rootSize b)^2, 0)
                     nj -> (i, nj)
    fillSquares b' (i', j')

fillSquareWith :: Board -> (Int, Int) -> Square -> Maybe Board
fillSquareWith (Board b) (i, j) s = case s of 
                                (Right x)     -> return $ Board b
                                (Left [])     -> Nothing
                                (Left [x]) -> do
                                  let (Board b') = update (Board b) (i, j) (Right x)
                                  cr <- clear (rowWise $ rootSize b') (i, 0) x (Board b')
                                  cc <- clear (colWise $ rootSize b') (0, j) x cr
                                  clear (boxWise $ rootSize b') (boxNo i, boxNo j) x cc 
                                l -> return $ update (Board b) (i, j) l
                              where boxNo n = (rootSize b) * (n `div` rootSize b)

update :: Board -> (Int, Int) -> Square -> Board
update (Board b) (i, j) s = Board $ b V.// [(i, r')]
  where r = b V.! i
        r' = r V.// [(j, s)]

possibleVals :: Board -> (Int, Int) -> Square
possibleVals (Board b) (i, j) = case (b V.! i V.! j) of 
                          (Right x) -> Right x
                          otherwise -> Left $ filterBox $ filterCol $ filterRow [1 .. (rootSize b)^2]

  where filterRow          = filter $ flip notElem $ V.map squareToInt $ b V.! i
        filterCol          = filter $ flip notElem $ V.map (squareToInt . (flip (V.!) $ j)) b
        filterBox          = filter $ flip notElem $ V.map squareToInt $ V.concat $ V.toList $ boxElements

        boxElements        = V.map (V.ifilter $ (\j' _ -> j' >= boxNo j && j' < boxNo j + rootSize b)) 
                           $ V.ifilter (\i' _ -> i' >= boxNo i && i' < boxNo i + rootSize b) $ b
                    where boxNo j = (rootSize b) * (j `div` rootSize b)

isSolved :: Board -> Bool
isSolved (Board b) = case V.mapM V.sequence b of 
               Right _ -> True
               Left _  -> False

rootSize :: Foldable t => t a -> Int
rootSize = round . sqrt . fromIntegral . length

-- IMPURE THROWS EXCEPTION IF BOARD IS SOLVED
findEmptySquare :: Board -> (Int, Int)
findEmptySquare (Board b) = aux firstLeft firstLeft
  where aux :: (Int, Int) -> (Int, Int) -> (Int, Int)
        aux (i, j) (bi, bj)
          | i == size - 1 && j == i = (bi, bj)
          | otherwise = case squ (i, j) of 
                          (Right _) -> aux (step (i, j)) (bi, bj)
                          (Left vs) 
                            | length vs == 2 -> (i, j)
                            | otherwise -> aux (step (i, j)) $ better (i, j) (bi, bj)
        firstLeft = until (isLeft . squ) step (0,0)
        step = boxWise size
        squ (i, j) = b V.! i V.! j
        size = V.length b
        better x y= case (compare `on` (length . fromLeft [] . squ)) x y of
                      LT -> x
                      _  -> y

successors :: Board -> [Board]
successors (Board b) = catMaybes $ mapValues $ findEmptySquare (Board b)
  where mapValues :: (Int, Int) -> [Maybe Board]
        mapValues (i, j) = map (\x -> fillSquareWith (Board b) (i, j) (Left [x])) $ fromLeft [] $ squ (i, j) 
        squ (i, j) = b V.! i V.! j

search :: (Board -> [Board]) -> (Board -> Bool) -> Board -> Maybe Board
search succ done init = bfSearch succ done $ H.singleton init
  where bfSearch :: (Board -> [Board]) -> (Board -> Bool) -> H.Heap Board -> Maybe Board
        bfSearch succ done heap = do
          (b, hs) <- H.uncons heap
          case done b of
            True -> return b
            False -> do
              let succs = succ b
              let succHeap = H.fromList succs
              let heap' = H.union succHeap hs
              bfSearch succ done heap'

solve :: RawBoard -> Maybe RawBoard
solve rb = toRawBoard <$> (search successors isSolved =<< fillSquares (toBoard rb) (0,0))



-- fillSquares :: Maybe Board -> (Int, Int) -> Maybe Board
-- fillSquares Nothing  _      = Nothing
-- fillSquares (Just b) (i, j) = undefined





pb :: Board -> IO ()
pb = mapM_ putStrLn . formatBoard . toRawBoard

b :: Board
b = toBoard $ head $ fromJust $ readBoards $ unsafePerformIO $ getJson "ten-boards.json"

fb = toBoard $ (map . map) (read . (\c -> [c])) $ [ "1003"
                                                  , "0000"
                                                  , "0020"
                                                  , "0000"
                                                  ]

nb = toBoard $ (map . map) (read . (\c -> [c])) $ [ "000005409"
                                                  , "451002300"
                                                  , "982000561"
                                                  , "607000980"
                                                  , "003460000"
                                                  , "500287010"
                                                  , "040070096"
                                                  , "300000700"
                                                  , "005946802"
                                                  ]

ez :: [[Int]]
ez = (map . map) (read . (\c -> [c])) ["230915000","000200540","607000000","001000009","890503017","500000600","000000905","016007000","000329001"]

-- try arc consistency and compare results
