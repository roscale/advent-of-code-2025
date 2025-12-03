import Data.Char
import Data.List
import Data.Tuple.Extra
import Safe

main = do
  list <- readFile "input.txt"
  let banks = map (map digitToInt) (lines list)

  print $ part1 banks
  print $ part2 banks

part1 banks = sum $ map (joltage 2) banks

part2 banks = sum $ map (joltage 12) banks

joltage n bank = cat batteries
  where
    cat = foldl (\acc b -> acc * 10 + b) 0
    batteries = map fst3 $ take n $ drop 1 $ iterate nextBattery (-1, bank, n)

nextBattery (battery, slice, n) = (b, s, n - 1)
  where
    b = maximum $ take (length slice - n + 1) slice
    i = elemIndexJust b $ init slice
    s = drop (i + 1) slice
