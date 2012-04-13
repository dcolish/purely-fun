import System.Random
import Data.List hiding (insert)
import Chapter3 (insert, Color(..), RB(..))


show_tree E' = return ()
show_tree t@(RB c a y z) = do
  root
  link t
  show_tree a
  show_tree z
  where
    root = case c of
      R -> putStrLn $ (show y) ++ "[style=filled, fillcolor=red];"
      B -> putStrLn $ (show y) ++ "[style=filled, fillcolor=black];"
    link a = case a of
      RB _ (RB _ _ x _) y (RB _ _ z _) -> do
        putStrLn $ (show y) ++ " -> " ++ (show x)
        putStrLn $ (show y) ++ " -> " ++ (show z)
      RB _ (RB _ _ x _) y E' -> do
        putStrLn $ (show y) ++ " -> " ++ (show x)
      RB _ E' y (RB _ _ z _) -> do
        putStrLn $ (show y) ++ " -> " ++ (show z)
      RB _ E' y E' -> return ()
      E' -> return ()


main = do
  putStrLn "digraph tree {"
  putStrLn "node [fontcolor=white, nodesep=1];"
  putStrLn "repulsiveforce=\"1\";"
  putStrLn "smoothing=\"spring\";"
  putStrLn "splines=\"true\";"
  seed <- newStdGen
  let nums = randomlist 100 seed
  show_tree $ build_tree nums E'
  putStrLn "}"
    where 
      build_tree [] t = t
      build_tree (x:xs) t = build_tree xs (insert x t)
      randomlist :: Int -> StdGen -> [Int]
      randomlist n = take n . randomRs (1, 1000)
