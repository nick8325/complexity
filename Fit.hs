import Numeric.LinearProgramming
import System.Environment
import Data.Tuple
import Data.List
import Data.Function
import Data.Ord

parse :: String -> [(Double, Double, Int)]
parse = map (triple . map read . words) . lines
  where
    triple [x,y,z] = (x,y,truncate z)

constraints f points =
  Dense [ f x y | (x, y, _) <- points ]

fitAbove trans points =
  simplex (Minimize (opt trans points))
          (constraints (\x y -> [x, 1] :=>: y) (rename (xt trans) points))
          [Free 2]
fitBelow trans points =
  simplex (Maximize (opt trans points))
          (constraints (\x y -> [x, 1] :<=: y) (rename (xt trans) points))
          [Free 2]

maxX points = maximum (map fst3 points)
fst3 (x,_,_) = x
snd3 (_,y,_) = y
thd3 (_,_,z) = z
preprocess maximum points =
  [ (x, maximum (map snd3 ps), sum (map thd3 ps))
  | ps@((x,_,_):_) <- groupBy ((==) `on` fst3) points ]
withoutOutliers ps =
  drop n (take (length ps - n) ps)
  where
    n = length ps `div` 10
expand = concatMap expand1
  where
    expand1 (x, y, n) = replicate n (x, y, 1)

--   int(at+b)
-- = a int(t) + bx
-- hence total area is:
--   a int(x) + bx - a int(0)
-- = a(int(x) - int(0)) + bx
opt trans points = [integral trans (maxX points) - integral trans 0, maxX points]
area (Known trans maxX a b _) = a * (integral trans maxX - integral trans 0) + b * maxX

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

rename f ps = [(f x, y, k) | (x, y, k) <- ps]

findArea trans sol maxX =
  case findSol sol of
    Just (_, [a, b]) ->
      Known trans maxX a b sol
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

main = do
  worst <- fmap parse (readFile "worst")
  best <- fmap parse (readFile "best")
  let points = (worst, best)
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

  putStrLn $ "Worst-case complexity: " ++ complexity (above theBest)
  putStrLn $ "Best-case complexity: " ++ complexity (below theBest)

  writeFile "gnuplot" . unlines $ [
    "set dummy n",
    "plot 'worst', 'best'" ++ concat
      [ ", " ++ formula x ++ " linewidth 2"
      | x <- [above theBest, below theBest] ]
    ]
