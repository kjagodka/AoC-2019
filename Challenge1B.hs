totalFuelRequiment mass =
  totalFuelRequimentHelper mass 0
  where
    totalFuelRequimentHelper newMass acc
      | newFuel == 0 = acc
      | otherwise    = totalFuelRequimentHelper newFuel (acc + newFuel)
      where
        newFuel = fuelRequiments newMass
        fuelRequiments mass = max fuel 0
          where
            fuel = (mass `div` 3) - 2

main = do
  input <- getContents
  let modules = map read (lines input) :: [Integer]
  print (sum (map totalFuelRequiment modules))
