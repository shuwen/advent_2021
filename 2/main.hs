import System.IO

main = do
  contents <- readFile "input.txt"
  let commands = (\[x, y] -> (x, read y :: Int)) . words <$> lines contents
  let (horizontal, depth) = processCommands commands
  -- part 1
  print $ horizontal * depth
  -- part 2
  let (horizontal2, depth2, _) = processCommands2 commands
  print $ horizontal2 * depth2

processCommands :: [(String, Int)] -> (Int, Int)
processCommands = foldr move (0, 0) . reverse

processCommands2 :: [(String, Int)] -> (Int, Int, Int)
processCommands2 = foldr move2 (0, 0, 0) . reverse

move :: (String, Int) -> (Int, Int) -> (Int, Int)
move (direction, units) (horizontal, depth)
  | direction == "forward" = (horizontal + units, depth)
  | direction == "down" = (horizontal, depth + units)
  | direction == "up" = (horizontal, depth - units)

move2 :: (String, Int) -> (Int, Int, Int) -> (Int, Int, Int)
move2 (direction, units) (horizontal, depth, aim)
  | direction == "forward" = (horizontal + units, depth + (units * aim), aim)
  | direction == "down" = (horizontal, depth, aim + units)
  | direction == "up" = (horizontal, depth, aim - units)