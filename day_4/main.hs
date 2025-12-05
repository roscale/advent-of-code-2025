import Safe

main = do
  list <- lines <$> readFile "input.txt"
  let grid = (length (head list), concat list)

  print $ part1 grid

part1 grid = length $ filter (forkliftable grid) $ enumerate grid

forkliftable grid (row, col) = isRoll grid (row, col) && rollsAround <= 4
  where
    isRoll grid (row, col) = get grid (row, col) == Just '@'
    rollsAround = length $ filter (== Just '@') surroundings
    surroundings = subGrid grid (row - 1, row + 1, col - 1, col + 1)

type Grid a = (Int, [a])

get :: Grid a -> (Int, Int) -> Maybe a
get (width, array) (row, col)
  | row >= 0 && row < width && col >= 0 && col < height = array `atMay` (row * width + col)
  | otherwise = Nothing
  where
    height = length array `div` width

coords width i = (i `div` width, i `mod` width)

enumerate (width, array) = map (coords width) [0 .. length array - 1]

subGrid grid (start_row, end_row, start_col, end_col) = [get grid (i, j) | i <- [start_row .. end_row], j <- [start_col .. end_col]]
