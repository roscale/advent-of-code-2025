import Data.Char
import Data.List
import Data.List.Extra
import Data.List.Split

main = do
  input <- readFile "input.txt"
  let matrix = lines input
  let problems = getProblems matrix

  print $ part1 problems
  print $ part2 problems

getProblems matrix = map transpose problems
  where
    problems = splitWhen (all isSpace) $ transpose matrix

part1 problems = sum $ map evaluate problems

part2 problems = sum $ map (evaluate . transposeProblem) problems

evaluate problem = foldl1 op terms
  where
    op = case last trimmedProblem of
      "*" -> (*)
      "+" -> (+)
    terms = map read $ init trimmedProblem
    trimmedProblem = map trim problem

transposeProblem problem = transpose (init problem) ++ [last problem]
