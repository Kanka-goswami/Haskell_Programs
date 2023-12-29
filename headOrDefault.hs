-- Return first element of the list or the given default value
headOrDefault :: Int -> [Int] -> Int
headOrDefault def list = if null list then def else head list