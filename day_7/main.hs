import Data.Set hiding (take)

main = do
  input <- readFile "input.txt"
  let matrix = lines input
  let (width, height) = (length (head matrix), length matrix)
  let array = concat matrix

  let (start : splitters) = Prelude.map fst $ Prelude.filter (\(_, e) -> e == 'S' || e == '^') $ zip (enumerate2D width) array

  print $ part1 height start (fromList splitters)

enumerate2D width = Prelude.map (coords width) [0 ..]
  where
    coords width i = (i `div` width, i `mod` width)

part1 :: Int -> (Int, Int) -> Set (Int, Int) -> Int
part1 height start splitters = fst $ last $ take height $ iterate (advanceBeams splitters) (0, fromList [start])

advanceBeams :: Set (Int, Int) -> (Int, Set (Int, Int)) -> (Int, Set (Int, Int))
advanceBeams splitters (splitCount, beams) = (newSplitCount, newBeams)
  where
    newSplitCount = splitCount + length (Data.Set.filter (\s -> length s == 2) newSplits)
    newBeams = foldl1 union newSplits
    newSplits = Data.Set.map (advanceBeam splitters) beams

advanceBeam :: Set (Int, Int) -> (Int, Int) -> Set (Int, Int)
advanceBeam splitters (i, j) =
  if (i + 1, j) `elem` splitters
    then fromList [(i + 1, j - 1), (i + 1, j + 1)]
    else fromList [(i + 1, j)]
