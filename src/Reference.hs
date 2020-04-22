
module Reference(reference) where

-- Haskell reference for example program...

reference :: IO ()
reference = do
  putStrLn $ "original(9) = " <> show (original 9)
  putStrLn $ "goal(9) = " <> show (goal 9)


original :: Int -> Int
original = \arg -> do
  let dub x = x + x
  let twice f x = f (f x)
  let increase q x = dub x + q
  twice (increase (arg+1)) 3


goal :: Int -> Int
goal = \arg -> do
  let q = arg + 1
  let x0 = 3
  let x1 = (x0 + x0) + q
  (x1 + x1) + q
