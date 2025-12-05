import Data.Set

main = do
  list <- lines <$> readFile "input.txt"
  let width = length (head list)
  let elements = concat list

  let rolls = fromList $ fmap fst $ Prelude.filter (\e -> snd e == '@') $ zip (enumerate (width, elements)) elements

  print $ part1 rolls
  print $ part2 rolls

part1 rolls = length $ Data.Set.filter (forkliftable rolls) rolls

part2 rolls = sum $ takeWhile (/= 0) (pairwise (-) $ length <$> iterate withoutForkliftables rolls)
  where
    withoutForkliftables rolls = rolls `difference` Data.Set.filter (forkliftable rolls) rolls

pairwise f xs = zipWith f xs (tail xs)

forkliftable rolls (row, col) = rollsAround <= 4
  where
    rollsAround = length $ rolls `intersection` surroundings
    surroundings = subGrid (row - 1, row + 1, col - 1, col + 1)

enumerate (width, array) = fmap (coords width) [0 .. length array - 1]

coords width i = (i `div` width, i `mod` width)

subGrid (start_row, end_row, start_col, end_col) = fromList [(i, j) | i <- [start_row .. end_row], j <- [start_col .. end_col]]
