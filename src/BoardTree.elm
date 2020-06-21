-- Jeffrey Huang and Oliver Tsang, Spring 2020
-- Senet in Elm (Course Project) for CS 223, Functional Programming
-- BoardTree.elm: define a helper data structure to hold the board efficiently
--
-- Types:
--   Tree a
--     allows O(log n) access and allows easy manipulation.
--     Based on a BST except we search over indices
--     but the elements are only stored in leafs.
--     *Note: does not support merging or insertion
--     (not needed for fixed-size senet board)
-- Functions:
--   fromList: builds a tree based from the given list
--   toList:   returns a list based on the tree (for easier reading)
--   getElem:  (tries to) return element at a given index
--   setElem:  (tries to) set the value of an element at a given index
--   swap:     (tries to) swap the values of two elements
--   map:      applies a function f to every element

module BoardTree exposing (Tree, fromList, toList, setElem, getElem, swap, map)

-- comparable is for the index
-- a is for the contents in the leaf nodes
type Tree a
  = Empty
  | Leaf Int a
  | Parent Int (Tree a) (Tree a)

-- build tree from list of sorted elements
fromList : List a -> Tree a
fromList xs =
  let
    fromListHelper i size ys =
      case ys of
        [] ->
          Empty
        y :: [] ->
          Leaf i y
        _ ->
          let
            m  = ceiling <| logBase 2 (toFloat size) -- max height of tree
            halfn = 2 ^ (m-1)
            j = i + halfn
            firstHalf = List.take halfn ys
            lastHalf = List.drop halfn ys
          in
            Parent j
              (fromListHelper i halfn firstHalf)
              (fromListHelper j (size-halfn) lastHalf)
  in
    fromListHelper 0 (List.length xs) xs

-- build a list from a tree (helps visualize)
toList : Tree a -> List a
toList t =
  case t of
    Empty ->
      []
    Leaf _ x ->
      [x]
    Parent _ t1 t2 ->
      (toList t1) ++ (toList t2)

-- O(log n) access time
-- return the element at index i if it exists
getElem : Int -> Tree a -> Maybe a
getElem i t =
  case t of
    Empty ->
      Nothing
    Leaf j x ->
      if i == j then
        Just x
      else
        Nothing
    Parent j t1 t2 ->
      if i < j then
        getElem i t1
      else
        getElem i t2

-- set x as the value at index i if i exists
setElem : Int -> a -> Tree a -> Maybe (Tree a)
setElem i x t =
  case t of
    Empty ->
      Nothing
    Leaf j _ ->
      if i == j then
        Just <| Leaf j x
      else
        Nothing
    Parent j t1 t2 ->
      if i < j then
        Maybe.map
          (\left -> (Parent j) left t2)
          (setElem i x t1)
      else
        Maybe.map
          (\right -> (Parent j) t1 right)
          (setElem i x t2)

-- swap the values at indices i and j
swap : Int -> Int -> Tree a -> Maybe (Tree a)
swap i j t =
  case t of
    Empty ->
      Nothing
    _ ->
      let
        mx = getElem i t
        my = getElem j t
        -- wrapper for the Maybes involved with setting the element
        setter k mw ms =
          ms |> Maybe.andThen (\s ->
          mw |> Maybe.andThen (\w ->
          setElem k w s))
      in
        Just t |> setter j mx |> setter i my

map : (a -> b) -> Tree a -> Tree b
map f t =
  case t of
    Empty ->
      Empty
    Leaf i x ->
      Leaf i (f x)
    Parent i left right ->
      Parent i (map f left) (map f right)