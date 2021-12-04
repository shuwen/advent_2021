import System.IO

main = do
  contents <- readFile "input.txt"
  let readings = map (read . pure :: Char -> Int) <$> lines contents

  -- part 1
  print $ getPowerConsumption readings

  -- part 2
  print $ getLifeSupport readings

getPowerConsumption :: [[Int]] -> Int
getPowerConsumption readings = gamma * epsilon
  where
    threshold = length readings `div` 2
    -- add the digits in each position together
    sums = foldr1 (zipWith (+)) readings
    -- if the digits sum up to more than half the list, 1 is more common, otherwise 0
    gamma = digitListToDec $ fromEnum . (> threshold) <$> sums
    -- inverse of the above
    epsilon = digitListToDec $ fromEnum . (< threshold) <$> sums

digitListToDec :: [Int] -> Int
digitListToDec = foldl1 (\acc x -> acc * 2 + x)

getLifeSupport :: [[Int]] -> Int
getLifeSupport readings = (digitListToDec . getOxygenGen $ readings) * (digitListToDec . getCO2Scrubber $ readings)

getOxygenGen :: [[Int]] -> [Int]
getOxygenGen readings
  | null readings = []
  | null (head readings) = []
  | otherwise = mostCommonHead : getOxygenGen (tail <$> filter (\r -> head r == mostCommonHead) readings)
  where
    mostCommonHead = getMostCommonHead readings

getMostCommonHead :: [[Int]] -> Int
getMostCommonHead readings = fromEnum $ sum (head <$> readings) >= threshold
  where
    threshold = length readings `div` 2 + length readings `mod` 2

getCO2Scrubber :: [[Int]] -> [Int]
getCO2Scrubber readings
  | null readings = []
  | null (head readings) = []
  | otherwise = leastCommonHead : getCO2Scrubber (tail <$> filter (\r -> head r == leastCommonHead) readings)
  where
    leastCommonHead = getLeastCommonHead readings

getLeastCommonHead :: [[Int]] -> Int
getLeastCommonHead readings
  | sumDigits == 0 = 0 -- all 0s
  | sumDigits == length readings = 1 -- all 1s
  | otherwise = fromEnum $ sum (head <$> readings) < threshold
  where
    sumDigits = sum (head <$> readings)
    threshold = length readings `div` 2 + length readings `mod` 2