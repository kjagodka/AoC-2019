totalFuelRequiment mass =
  totalFuelRequimentHelper mass 0
  where
    totalFuelRequimentHelper newMass acc
      | newFuel > 0 = totalFuelRequimentHelper newFuel (acc + newFuel)
      | otherwise   = acc
      where
        newFuel = fuelRequiments newMass
        fuelRequiments mass =
          if fuel < 0
            then 0
            else fuel
          where
            fuel = (mass `div` 3) - 2

main = do
  userInput <- getContents
  let modules = map read (lines userInput) :: [Integer]
  print (sum (map totalFuelRequiment modules))
