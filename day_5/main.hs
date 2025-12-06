import Data.List.Extra

main = do
  input <- readFile "input.txt"
  let [rangesStr, idsStr] = splitOn "\n\n" input

  let ranges :: [(Int, Int)] = map parseRange $ splitOn "\n" rangesStr
  let ids :: [Int] = map read $ splitOn "\n" idsStr

  print $ part1 ranges ids

part1 ranges ids = length $ filter (isFresh ranges) ids

parseRange str = (x, y)
  where
    [x, y] = map read $ splitOn "-" str

isFresh ranges id = any (inRange id) ranges

inRange elem (x, y) = x <= elem && elem <= y
