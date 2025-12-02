import Data.List.Extra

expandRanges :: String -> [String]
expandRanges = concatMap (expandRange . splitOn "-") . splitOn ","
  where
    expandRange [a, b] = map show [read a :: Int .. read b]

isInvalid :: String -> Bool
isInvalid id = uncurry (==) $ splitAt (length id `div` 2) id

part1 = sum . map read . filter isInvalid

main = do
  input <- readFile "input.txt"

  let ids = expandRanges input

  print $ part1 ids
