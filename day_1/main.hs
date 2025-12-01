main = do
  input <- readFile "input.txt"

  let list = rotations input

  print $ part1 list
  print $ part2 list

rotations = map (convert . splitAt 1) . lines
  where
    convert (dir, dist) = (dir, read dist)

part1 list = length pointing_at_0
  where
    turns = scanl turn 50 list
    pointing_at_0 = filter (\x -> x `mod` 100 == 0) turns

turn pos (dir, dist) = pos + dist * vec
  where
    vec = case dir of
      "L" -> -1
      "R" -> 1

part2 = part1 . concatMap (\(dir, dist) -> replicate dist (dir, 1))
