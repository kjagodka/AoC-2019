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

do1202fix program =
  listReplace 1 12 $ listReplace 2 2 program

main = do
  input <- getLine
  let program = map read $ splitOn "," input
  let fixedProgram = do1202fix program
  print $ head $ compute 0 fixedProgram
