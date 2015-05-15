import Numeric.LinearProgramming
import System.Environment
import Data.Tuple
import Data.List
import Data.Function
import Data.Ord
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad

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
level (Known trans _ a _ _) = level_ trans

complexity :: Fit1 -> String
complexity (Known trans _ a _ _) = "O(" ++ complexity_ trans ++ ")"

formula fit@(Known trans _ a b _)
  | level fit == 0 = show (a + b)
  | otherwise = show a ++ " * " ++ formula_ trans ++ " + " ++ show b

bestOf :: (Fit1 -> Bool) -> [Fit] -> Fit
bestOf p fs =
  head . sortBy (comparing area_) $
    [ Fit a b
    | a@Known{} <- map above fs,
      b@Known{} <- map below fs,
      p a, p b ]
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

conT = Transformation 0 "1" "1" (const 1) id
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

tight :: Fit1 -> Bool
tight fit | level fit == 0 = True
tight fit@(Known trans maxX a b _) =
  abs (maxY - minY) > abs (0.10 * minY)
  where
    minY = a * xt trans 0 + b
    maxY = a * xt trans maxX + b

run1 chatty p input = do
  let points = (input, input)
      con = fit conT points
      lin = fit idT points
      logg = fit logT points
      nlogn = fit nlognT points
      quad = fit n2T points
      theBest = bestOf p [con, lin, logg, nlogn, quad]

  when chatty $ do
    putStrLn "=== Constant fit"
    print con
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

run n input = do
  prelBest <- run1 False (const True) input
  let badness (x, y) = abs ((eval (above prelBest) x - y) / eval (above prelBest) x)
      input' = drop n (sortBy (comparing badness) input)
  run1 True tight input'

processPoints :: [(Double, Double)] -> [(Double, Double)] -> [(Double, Double)]
processPoints worst best = sort $ worst ++ prunedBest
  where
    findMins :: [(Double, Double)] -> Map Double Double -> Map Double Double
    findMins [] map = map
    findMins ((x, y) : rest) map =
      let map' =
            case Map.lookup x map of
              Nothing -> Map.insert x y map
              Just y' -> if y < y' then Map.insert x y map else map
      in findMins rest map'

    worstMins = findMins worst Map.empty
    prunedBest = [ (x, y) | (x, y) <- best,
                            let y' = Map.findWithDefault y x worstMins,
                            y <= y' ]

main = do
  [name, outliers] <- getArgs

  let dataWorstFile = "data_worst_" ++ name
  let dataBestFile = "data_best_" ++ name
  let dataPrunedFile = "data_pruned_" ++ name

  -- Write the raw data without the complexity curves.
  writeFile ("gnuplot_raw_" ++ name) . unlines $ [
    "set dummy n",
    "plot '" ++ dataWorstFile ++ "','" ++ dataBestFile ++ "'"
    ]

  inputWorst <- fmap parse (readFile dataWorstFile)
  inputBest <- fmap parse (readFile dataBestFile)

  let input = processPoints inputWorst inputBest
  theBest <- run (read outliers) input

  putStrLn $ "Worst-case complexity: " ++ complexity (above theBest)
  putStrLn $ "Best-case complexity: " ++ complexity (below theBest)

  -- Write the pruned points to a file.
  writeFile dataPrunedFile . unlines $ [ show x ++ " " ++ show y  | (x, y) <- input ]

  writeFile ("gnuplot_" ++ name) . unlines $ [
    "set dummy n",
    "plot '" ++ dataPrunedFile ++ "'," ++
      formula (above theBest) ++ " linewidth 2," ++
      formula (below theBest) ++ " linewidth 2"
    ]
