fuelPerModule :: Integer -> Integer
fuelPerModule moduleMass = (moduleMass `div` 3) - 2

main = do
  input <- getContents
  let modules = map read (lines input) :: [Integer]
  print (sum (map fuelPerModule modules))
