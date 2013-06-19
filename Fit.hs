import Numeric.LinearProgramming
import System.Environment

parse :: String -> [(Double, Double)]
parse = map (pair . map read . words) . lines
  where
    pair [x,y,z] = (x,y)

constraints f points =
  Dense [ f x y | (x, y) <- points ]

fitAbove trans points =
  simplex (Minimize (opt trans points))
          (constraints (\x y -> [x, 1] :=>: y) (rename (xt trans) points))
          [Free 2]
fitBelow trans points =
  simplex (Maximize (opt trans points))
          (constraints (\x y -> [x, 1] :<=: y) (rename (xt trans) points))
          [Free 2]

--   int(at+b)
-- = a int(t) + bx
-- hence total area is:
--   a int(x) + bx - a int(0)
-- = a(int(x) - int(0)) + bx
opt trans points = [integral trans maxX - integral trans 0, maxX]
  where
    maxX = maximum (map fst points)

data Transformation = Transformation {
  name :: String,
  xt :: Double -> Double,
  integral :: Double -> Double
  }

data Fit = Fit { above :: Fit1, below :: Fit1 }
data Fit1 = Unknown Solution | Known String Double Double Double Solution

formula (Known name _ a b _) =
  show a ++ " * " ++ name ++ " + " ++ show b

best :: Fit -> Fit -> Fit
best (Fit a1 b1) (Fit a2 b2) =
  Fit (best1 (<=) a1 a2)
      (best1 (>=) b1 b2)

best1 :: (Double -> Double -> Bool) -> Fit1 -> Fit1 -> Fit1
best1 cmp x Unknown{} = x
best1 cmp Unknown{} y = y
best1 cmp x@(Known _ ax _ _ _) y@(Known _ ay _ _ _)
  | cmp ax ay = x
  | otherwise = y

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
  show k@(Known name area a b sol) =
    unlines [
      "form = " ++ formula k,
      "area = " ++ show area,
      " sol = " ++ show sol
      ]

idT = Transformation "x" id (\x -> x^2 / 2)
logT = Transformation "log(x)" (\x -> log (x+1))
       (\x -> (x+1) * log (x+1) - x)
nlognT = Transformation "x*log(x)" (\x -> x * log (x+1))
         (\x -> (x**2 - 1) * log (x+1) / 2 - (x-2)*x / 4)
n2T = Transformation "x**2" (^2) (\x -> x^3 / 3)

rename f ps = [(f x, y) | (x, y) <- ps]

findArea trans sol points =
  case findSol sol of
    Just (_, [a, b]) ->
      Known (name trans)
            (a * integral trans maxX + b * maxX)
            a b sol
    Nothing ->
      Unknown sol
  where
    findSol (Feasible x) = Just x
    findSol (Infeasible x) = Just x
    findSol (Optimal x) = Just x
    findSol _ = Nothing

    maxX = maximum (map fst points)

fit trans points =
  Fit
    (findArea trans (fitAbove trans points) points)
    (findArea trans (fitBelow trans points) points)

main = do
  args <- getArgs
  let filename = head (args ++ ["data"])
  points <- fmap parse (readFile filename)
  let lin = fit idT points
      logg = fit logT points
      nlogn = fit nlognT points
      quad = fit n2T points
      theBest = foldr1 best [lin, logg, nlogn, quad]

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

  writeFile "gnuplot" . unlines $ [
    "set term pngcairo",
    "set output \"graph.png\"",
    "plot '" ++ filename ++ "'" ++ concat
      [ ", " ++ formula x ++ " linewidth 5"
      | x <- [above theBest, below theBest] ]
    ]