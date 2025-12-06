import Data.Function
import Data.List.Extra

main = do
  input <- readFile "input.txt"
  let [rangesStr, idsStr] = splitOn "\n\n" input

  let ranges :: [(Int, Int)] = map parseRange $ splitOn "\n" rangesStr
  let ids :: [Int] = map read $ splitOn "\n" idsStr

  print $ part1 ranges ids
  print $ part2 ranges

parseRange str = (x, y)
  where
    [x, y] = map read $ splitOn "-" str

part1 ranges ids = length $ filter (isFresh ranges) ids
  where
    isFresh ranges id = any (inRange id) ranges

part2 ranges = sum $ map rangeLength disjoint
  where
    disjoint = foldl joinRange [] biggestToSmallest
    biggestToSmallest = sortBy (flip compare `on` rangeLength) ranges

inRange elem (x, y) = x <= elem && elem <= y

rangeLength (x, y) = y - x + 1

-- Precondition: all (\r -> rangeLength (x, y) <= rangeLength r) disjoint
joinRange disjoint (x, y) = if newY >= newX then (newX, newY) : disjoint else disjoint
  where
    newX = case find (inRange x) disjoint of
      Just range -> snd range + 1
      Nothing -> x

    newY = case find (inRange y) disjoint of
      Just range -> fst range - 1
      Nothing -> y
