import System.IO

main = do
  contents <- readFile "input.txt"
  let depths = map (\x -> read x :: Int) $ lines contents
  -- part 1
  print $ numIncreases depths
  -- part 2
  print $ numIncreases $ makeWindows depths

numIncreases :: [Int] -> Int
numIncreases depths =
  foldl (\acc (x, y) -> if x < y then acc + 1 else acc) 0 $ (zip <*> tail) depths

makeWindows :: [Int] -> [Int]
makeWindows depths = map (\(x, y, z) -> x + y + z) $ (zip3 <*> tail <*> tail . tail) depths