import System.Random
import Data.List hiding (insert)
import Chapter3 (insert, Color(..), RB(..))

show_tree E' = return ()
show_tree (RB c a y z) = do
  root
  link a
  show_tree a 
  link z
  show_tree z
  where
    root = case c of
      R -> putStrLn $ (show y) ++ "[style=filled, fillcolor=red];"
      B -> putStrLn $ (show y) ++ "[style=filled, fillcolor=black];"
    link a = case a of
      RB _ a' y' z' -> do
        putStrLn $ (show y) ++ " -> " ++ (show y')
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
      randomlist n = take n . randomRs (1,1000)
