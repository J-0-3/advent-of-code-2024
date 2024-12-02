import Data.IntMap qualified as IntMap
import Data.List

toInt :: String -> Int
toInt = read

similarityMetrics' :: Int -> IntMap.IntMap Int -> [Int] -> [Int] -> Int
similarityMetrics' total _ [] _ = total
similarityMetrics' total cache (leftH : leftT) right =
  similarityMetrics' (total + leftH * right_count) cache leftT right
  where
    new_cache = case cached_value of
      Nothing -> IntMap.insert leftH right_count cache
      Just x -> cache
    right_count = case cached_value of
      Nothing -> length (filter (== leftH) right)
      Just x -> x
    cached_value = IntMap.lookup leftH cache

similarityMetrics :: [Int] -> [Int] -> Int
similarityMetrics = similarityMetrics' 0 IntMap.empty

main :: IO ()
main = do
  input <- readFile "input.txt"
  let [left, right] = sort . transpose $ fmap toInt . words <$> lines input
  let similarity = similarityMetrics left right
  print similarity
