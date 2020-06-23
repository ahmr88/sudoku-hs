{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
module Board where 

import GHC.Generics
import qualified Data.ByteString.Lazy as B
import Data.Aeson
import Data.List
import Data.Function (on)

type RawBoard = [[Int]]

data BoardData = BoardData { boards :: [RawBoard]
                           } deriving (Show,Generic)

instance FromJSON BoardData

instance ToJSON BoardData

jsonFile :: FilePath
jsonFile = "ten-boards.json"

getJson :: String -> IO B.ByteString
getJson = B.readFile

prr :: Maybe [RawBoard] -> IO ()
prr Nothing = putStrLn "invalid"
prr (Just bs) = mapM_ (\b -> putStrLn "" >> printBoard b) bs

readBoards :: B.ByteString -> Maybe [RawBoard]
readBoards = (=<<) (return . boards) . decode

printBoard :: RawBoard -> IO ()
printBoard = mapM_ putStrLn . formatBoard 

formatBoard :: RawBoard -> [String]
formatBoard b = foldl1 (\r3 r3' -> r3 ++ [margin] ++ r3') sep
  where sep = groupBox $ map formatRow b
        margin = ' ' : (replicate n '-')
        n      = sz^2 + (sz - 1) * 3 where sz = (length sep)

formatRow :: [Int] -> String
formatRow [] = ""
formatRow b = out
  where sep = groupBox b
        out = foldl1 (\r r' -> r ++ "|" ++ r') 
            $ map (\r -> " " ++ (intercalate "" $ map show r) ++ " ") sep

printRow :: [Int] -> IO ()
printRow = putStrLn . formatRow

groupBox :: [a] -> [[a]]
groupBox b = (map . map) snd $ groupBy ((==) `on` (box . fst)) $ zip [0..] b
  where box i = i `div` (round $ sqrt $ fromIntegral $ length b) 



