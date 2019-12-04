import Data.List

hasDouble [] = False
hasDouble (a:[]) = False
hasDouble (a:string)
  | a == (head string) = True
  | otherwise          = hasDouble (string)


isNonDecreasing [] = True
isNonDecreasing (a:[]) = True
isNonDecreasing (a:string)
  | a > (head string) = False
  | otherwise         = isNonDecreasing string




main =
  let passwords = map show [109165..576723] in
  let validPasswords = filter hasDouble $ filter isNonDecreasing passwords in
  print $ length validPasswords
