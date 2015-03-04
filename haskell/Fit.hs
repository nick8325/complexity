import Numeric.LinearProgramming
import System.Environment
import Data.Tuple
import Data.List
import Data.Function
import Data.Ord

parse :: String -> [(Double, Double)]
parse = map (pair . map read . words) . lines
  where
    pair [x,y] = (x,y)

constraints f points =
  Dense [ f x y | (x, y) <- points ]

fitAbove trans points =
  simplex (Minimize (opt trans (maxX points)))
          (constraints (\x y -> [x, 1] :>=: y) (rename (xt trans) points))
          [Free 2]
fitBelow trans points =
  simplex (Maximize (opt trans (maxX points)))
          (constraints (\x y -> [x, 1] :<=: y) (rename (xt trans) points))
          [Free 2]

maxX points = maximum (map fst points)
preprocess maximum points =
  [ (x, maximum (map snd ps))
  | ps@((x,_):_) <- groupBy ((==) `on` fst) (sortBy (comparing fst) points) ]

--   int(at+b)
-- = a int(t) + bx
-- hence total area is:
--   a int(x) + bx - a int(0)
-- = a(int(x) - int(0)) + bx

-- Tweak:
-- We would like the curve to fit more tightly on the right-hand side of
-- the graph, as this is what tells us the growth rate of the function.
-- I tried minimising the integral from X/2 to X, instead of from 0 to X,
-- but this overfits in some cases - we want the curve to at least somewhat
-- fit the left-hand side of the graph too. Instead I take the integral from
-- 0 to X, and the integral from X/2 to X, and add them.
opt trans maxX =
  zipWith (+)
    [integral trans maxX - integral trans 0, maxX]
    [integral trans maxX - integral trans (maxX/2), maxX/2]
area (Known trans maxX a b _) = a * x + b * y
  where
    [x, y] = opt trans maxX

eval :: Fit1 -> Double -> Double
eval (Known t _ a b _) x = a * xt t x + b

data Transformation = Transformation {
  level_ :: Int,
  complexity_ :: String,
  formula_ :: String,
  xt :: Double -> Double,
  integral :: Double -> Double
  }

data Fit = Fit { above :: Fit1, below :: Fit1 }
data Fit1 = Unknown Solution | Known Transformation Double Double Double Solution

level :: Fit1 -> Int
level (Known trans _ a _ _)
  | a == 0 = 0
  | otherwise = level_ trans

complexity :: Fit1 -> String
complexity (Known trans _ a _ _)
  | a == 0 = "O(1)"
  | otherwise = "O(" ++ complexity_ trans ++ ")"

formula (Known trans _ a b _)
  | a == 0 = show b
  | otherwise = show a ++ " * " ++ formula_ trans ++ " + " ++ show b

bestOf :: [Fit] -> Fit
bestOf fs =
  head . sortBy (comparing area_) $
    [ Fit a b
    | a@Known{} <- map above fs,
      b@Known{} <- map below fs,
      level a >= level b ]
  where
    area_ (Fit above below) = area above - area below

instance Show Fit where
  show (Fit above below) =
    unlines [
      "From above:",
      show above,
      "From below:",
      show below
      ]

instance Show Fit1 where
  show (Unknown sol) = " sol = " ++ show sol
  show k@(Known _ _ a b sol) =
    unlines [
      "form = " ++ formula k,
      "area = " ++ show (area k),
      " sol = " ++ show sol
      ]

idT = Transformation 2 "n" "n" id (\x -> x^2 / 2)
logT = Transformation 1 "log n" "log(n)" (\x -> log (x+1))
       (\x -> (x+1) * log (x+1) - x)
nlognT = Transformation 3 "n log n" "n*log(n)" (\x -> x * log (x+1))
         (\x -> (x**2 - 1) * log (x+1) / 2 - (x-2)*x / 4)
n2T = Transformation 4 "n^2" "n**2" (^2) (\x -> x^3 / 3)

rename f ps = [(f x, y) | (x, y) <- ps]

findArea trans sol maxX =
  case findSol sol of
    Just (_, [a, b]) ->
      let
        (a', b')
          | maxY - minY <= 0.05 * minY = (0, maxY)
          | otherwise = (a, b)
        minY = xt trans 0
        maxY = xt trans maxX
      in
        Known trans maxX a' b' sol
    Nothing ->
      Unknown sol
  where
    findSol (Feasible x) = Just x
    findSol (Infeasible x) = Just x
    findSol (Optimal x) = Just x
    findSol _ = Nothing

fit trans (worst, best) =
  Fit
    (findArea trans (fitAbove trans pointsAbove) (maxX worst))
    (findArea trans (fitBelow trans pointsBelow) (maxX best))
  where
    pointsAbove = preprocess maximum points
    pointsBelow = preprocess minimum points
    points = worst ++ best

run1 input = do
  let points = (input, input)
      lin = fit idT points
      logg = fit logT points
      nlogn = fit nlognT points
      quad = fit n2T points
      theBest = bestOf [lin, logg, nlogn, quad]

  putStrLn "=== Linear fit"
  print lin
  putStrLn "=== Logarithmic fit"
  print logg
  putStrLn "=== n log n fit"
  print nlogn
  putStrLn "=== Quadratic fit"
  print quad

  putStrLn "=== Best fit"
  print theBest

  return theBest

removeOutlier :: Fit1 -> [(Double, Double)] -> [(Double, Double)]
removeOutlier fit xs = drop 1 (sortBy (comparing badness) xs)
  where
    badness (x, y)
      | eval fit x == 0 = 0
      | otherwise = abs ((eval fit x - y) / eval fit x)

run 0 input = run1 input
run n input = do
  theBest <- run1 input
  run (n-1) (removeOutlier (above theBest) input)

main = do
  [outliers] <- getArgs

  -- Write the raw data without the complexity curves.
  writeFile "gnuplot_raw" . unlines $ [
    "set dummy n",
    "plot 'data'"
    ]

  input <- fmap parse (readFile "data")
  theBest <- run (read outliers) input

  putStrLn $ "Worst-case complexity: " ++ complexity (above theBest)
  putStrLn $ "Best-case complexity: " ++ complexity (below theBest)

  writeFile "gnuplot" . unlines $ [
    "set dummy n",
    "plot 'data'" ++ concat
      [ ", " ++ formula x ++ " linewidth 2"
      | x <- [above theBest, below theBest] ]
    ]
