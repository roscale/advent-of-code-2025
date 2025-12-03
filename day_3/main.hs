import Data.Char
import Data.List
import Safe

main = do
  list <- readFile "input.txt"
  let banks = map (map digitToInt) (lines list)

  print $ part1 banks

part1 banks = sum $ map joltage banks

joltage bank = battery1 * 10 + battery2
  where
    battery1 = maximum (init bank)
    battery1Index = elemIndexJust battery1 (init bank)
    battery2 = maximum $ drop (battery1Index + 1) bank
