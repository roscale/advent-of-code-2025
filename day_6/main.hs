import Data.List

main = do
  input <- readFile "input.txt"
  let matrix = transpose $ map words $ lines input
  let problems = map parseProblem matrix

  print $ part1 problems

part1 problems = sum $ map evaluate problems

parseProblem list = (op, terms)
  where
    op = case last list of
      "*" -> (*)
      "+" -> (+)
    terms = map read $ init list

evaluate (op, terms) = foldl1 op terms
