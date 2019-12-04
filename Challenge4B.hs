import Data.List

hasProperDouble string =
  helper string '0' 0
  where
    helper [] lastChar lastCharCount = (lastCharCount == 2)
    helper (c:string) lastChar lastCharCount
      | c == lastChar      = helper string lastChar (lastCharCount + 1)
      | lastCharCount == 2 = True
      | otherwise          = helper string c 1



isNonDecreasing [] = True
isNonDecreasing (a:[]) = True
isNonDecreasing (a:string)
  | a > (head string) = False
  | otherwise         = isNonDecreasing string




main =
  let passwords = map show [109165..576723] in
  let validPasswords = filter hasProperDouble $ filter isNonDecreasing passwords in
  print $ length validPasswords
