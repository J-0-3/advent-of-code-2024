import Data.List

toInt :: String -> Int
toInt = read

intDistance :: Int -> Int -> Int
intDistance x y = abs (x - y)

main :: IO ()
main = do
  input <- readFile "input.txt"
  let [left, right] = sort . transpose $ fmap toInt . words <$> lines input
  let distance = sum $ zipWith intDistance left right
  print distance
