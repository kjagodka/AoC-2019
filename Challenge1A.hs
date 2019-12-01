fuelPerModule :: Integer -> Integer
fuelPerModule moduleMass = (moduleMass `div` 3) - 2

main = do
  userInput <- getContents
  let modules = map read (lines userInput) :: [Integer]
  print (sum (map fuelPerModule modules))
