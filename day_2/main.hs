import Data.List.Extra

parse :: String -> [Int]
parse = concatMap (toRange . splitOn "-") . splitOn ","
  where
    toRange [a, b] = [read a .. read b]

isInvalid :: Int -> Bool
isInvalid id = uncurry (==) $ splitAt (length idStr `div` 2) idStr
  where
    idStr = show id

part1 = sum . filter isInvalid

main = do
  input <- readFile "input.txt"

  let ids = parse input

  print $ part1 ids
