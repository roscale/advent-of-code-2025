import Data.Map
import Data.Set

main = do
  input <- readFile "input.txt"
  let matrix = lines input
  let (width, height) = (length (head matrix), length matrix)
  let array = concat matrix

  let (start : splitters) = Prelude.map fst $ Prelude.filter (\(_, e) -> e == 'S' || e == '^') $ zip (enumerate2D width) array
  let splittersSet = Data.Set.fromList splitters

  print $ part1 height start splittersSet
  print $ part2 height start splittersSet

enumerate2D width = Prelude.map coords [0 ..]
  where
    coords i = (i `div` width, i `mod` width)

part1 height start splitters = fst $ simulate height start splitters

part2 height start splitters = sum $ Data.Map.elems $ snd $ simulate height start splitters

simulate :: Int -> (Int, Int) -> Set (Int, Int) -> (Int, Map (Int, Int) Int)
simulate height start splitters = last $ Prelude.take height $ iterate (advanceBeams splitters) initialState
  where
    initialState = (0, Data.Map.fromList [(start, 1)])

advanceBeams :: Set (Int, Int) -> (Int, Map (Int, Int) Int) -> (Int, Map (Int, Int) Int)
advanceBeams splitters (splitCount, beams) = (newSplitCount, mergedBeams)
  where
    newSplitCount = splitCount + length newSplits
    newSplits = Data.Map.filter (\s -> length s == 2) newBeams
    mergedBeams = Data.Map.unionsWith (+) newBeams
    newBeams = Data.Map.mapWithKey (advanceBeam splitters) beams

advanceBeam :: Set (Int, Int) -> (Int, Int) -> Int -> Map (Int, Int) Int
advanceBeam splitters (i, j) superimposed =
  if (i + 1, j) `elem` splitters
    then Data.Map.fromList [((i + 1, j - 1), superimposed), ((i + 1, j + 1), superimposed)]
    else Data.Map.fromList [((i + 1, j), superimposed)]
