import Numeric.LinearProgramming

parse :: String -> [(Double, Double)]
parse = map (pair . map read . words) . lines
  where
    pair [x,y,z] = (x,y)

above x y = [x, 1] :=>: y
below x y = [x, 1] :<=: y

constraints f points =
  Dense [ f x y | (x, y) <- points ]

fitAbove points =
  simplex (Minimize (area points)) (constraints above points) [Free 2]
fitBelow points =
  simplex (Maximize (area points)) (constraints below points) [Free 2]

-- y-value ranges from b to ax+b
-- hence average y-value is: 
--   (b+(ax+b))/2
-- = (ax+2b)/2
area points = [maxX, 2]
  where
    maxX = maximum (map fst points)

main = do
  points <- fmap parse (readFile "data")
  print (fitAbove points)
  print (fitBelow points)