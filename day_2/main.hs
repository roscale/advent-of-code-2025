import Data.List.Extra

main = do
  input <- readFile "input.txt"

  let ids = expandRanges input

  print $ part1 ids

expandRanges :: String -> [String]
expandRanges = concatMap (expandRange . splitOn "-") . splitOn ","
  where
    expandRange [a, b] = map show [read a :: Int .. read b]

invalidSum predicate = sum . map read . filter predicate

part1 = invalidSum repeatedTwice
  where
    repeatedTwice id = uncurry (==) $ splitAt (length id `div` 2) id
