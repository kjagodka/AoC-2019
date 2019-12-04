import Data.List.Split

listReplace index newValue list =
  (take index list) ++ [newValue] ++ (drop (index + 1) list)

doOperation opcode arg1 arg2 =
  if opcode == 1
    then arg1 + arg2
    else arg1 * arg2

compute pc state =
  let opcode = state !! pc in
  if opcode == 99
    then
      state
    else
      let arg1 = state !! (state !! (pc + 1)) in
      let arg2 = state !! (state !! (pc + 2)) in
      let resultPtr = state !! (pc + 3) in
      let result = doOperation opcode arg1 arg2 in
      let newState = listReplace resultPtr result state in
      compute (pc + 4) newState

apply program noun verb =
  listReplace 1 noun $ listReplace 2 verb program

doSimulation program (noun, verb) =
  let applied = apply program noun verb in
  head $ compute 0 applied

main = do
  input <- getLine
  wantedOutput <- getLine
  let program = map read $ splitOn "," input
  let parameters = [(noun, verb) | noun <- [1..], verb <- [1..noun]]
  let wantedOutputParsed = read wantedOutput
  print $ head $ filter (\param -> doSimulation program param == wantedOutputParsed) parameters
