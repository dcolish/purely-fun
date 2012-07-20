{-# LANGUAGE BangPatterns #-}

import System.Environment
import System.Random
import Data.List hiding (insert)
import Chapter3 (
  deleteMin,
  findMin,
  insert,
  member,
  Color(..),
  LeftistHeap(..),
  RB(..))


randomlist :: Int -> StdGen -> [Int]
randomlist n = take n . randomRs (1, 100000)


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


test_tree = do
  -- f <- getArgs
  words <- readFile "/dev/stdin"
  show_tree $ build_tree (lines words) E'
    where 
      build_tree [] t = t
      build_tree (x:xs) t = build_tree xs (insert x t)


show_heap E = return ()
show_heap h@(LeftistHeap r x a b) = do
  root
  link h
  show_heap a
  show_heap b
  where
    root = putStrLn $ (show x) ++  "[style=filled, fillcolor=black];"
    link h = case h of
      LeftistHeap _ x (LeftistHeap _ y _ _) (LeftistHeap _ z _ _) -> do
        putStrLn $ (show x) ++ " -> " ++ (show y)
        putStrLn $ (show x) ++ " -> " ++ (show z)
      LeftistHeap _ x (LeftistHeap _ y _ _) E -> do
        putStrLn $ (show x) ++ " -> " ++ (show y)
      LeftistHeap _ x E (LeftistHeap _ z _ _) -> do
        putStrLn $ (show x) ++ " -> " ++ (show z)
      LeftistHeap _ x E E -> return ()
      E -> return ()


-- There will be duplicates, this might lead to some confusion in the visualization
test_left_heap = do
  seed <- newStdGen
  let nums = randomlist 1000000 seed
  putStrLn $ show $ sort $ build_heap nums E
  where
    build_heap [] h = h
    -- build_heap (x:xs) h = build_heap xs (insert x h)
    build_heap (x:xs) h = let !newH = insert x h in build_heap xs newH
    sort E = []
    -- sort h =  findMin h : sort (deleteMin h)
    sort h = let !newH = deleteMin h in (findMin h) : sort newH


show' fn = do
  putStrLn "digraph tree {"
  putStrLn "node [fontcolor=white, nodesep=1];"
  putStrLn "repulsiveforce=\"1\";"
  putStrLn "smoothing=\"spring\";"
  putStrLn "splines=\"false\";"
  fn
  putStrLn "}"


quicksort [] = []
quicksort (x:xs) = (quicksort lesser) ++ [x] ++ (quicksort greater)
  where
    lesser = filter (< x) xs
    greater = filter (> x) xs


test_quick_sort = do
  seed <- newStdGen
  let nums = randomlist 1000000 seed
  putStrLn $ show $ quicksort $ nums


main = do
  -- test_quick_sort
  -- show' 
  test_left_heap
  -- show' test_tree
