-- Jeffrey Huang and Oliver Tsang, Spring 2020
-- Senet in Elm (Course Project) for CS 223, Functional Programming
-- AI.elm: an artificial intelligence player
-- currently based on minimax and alpha-beta pruning

module AI exposing (..)

import BoardTree as BT exposing (..)
import Board exposing (..)
import Logic exposing (..)


------ Laziness support ------
-- Without memoization because
-- the results are subject to change

type alias Thunk a = () -> a

force : Thunk a -> a
force thunk =
  thunk ()

-- lazy : (() -> a) -> Thunk a
-- lazy = Thunk

------ Move tree definitions ------
-- eager for now
-- also missing the value for now
-- value will be evaluated lazily later...
type MoveTree -- comparable
  -- = L comparable GameState -- leaf
  = L GameState
  | T
      -- comparable -- value
      GameState  -- current gs
      MoveArray  -- next moves

type alias MoveArray =
  BT.Tree (List (Maybe Pawn, MoveTree))

------ Minimax ------

-- TODO:
--   1. Handle game termination situation
--   2. Control how many steps to look forward
--   3. Handle case where there are no legal moves so you need to skip forward...
--   4. Make lazy !




-- 0 is the best possible score for black
-- since it means black has promoted everything
leafVal : GameState -> Int
leafVal gs =
  let
    -- arbitrarily set black to negative
    -- so that it evaluates from black's perspective
    sign sq =
      case sq of
        Free      -> 0
        Occ Black -> -1
        Occ White -> 1

    rebirthSquare =
      (lastFreeBy 14 gs)
      |> Maybe.withDefault 0

    -- Value Function
    -- Choose values based on distance from the end
    valf : Int -> SquareState -> Int
    valf i sq =
      case squareType i of
        Spec Horus ->
          -- basically as good as having promoted it
          0
        Spec Reatoum ->
          -- worst case is demotion
          (sign sq) * (30 - rebirthSquare)
        Spec Truths ->
          -- worst case is demotion
          (sign sq) * (30 - rebirthSquare)
        _ ->
          (sign sq) * (30-i)
  in
    -- could maybe make more efficient with some
    -- kind of enumerate-toList/map type thing
    List.range 0 29        |> List.map  (\i ->
    BT.getElem i gs.board  |> Maybe.map (\sq ->
    valf i sq)
    |> Maybe.withDefault 0)
    |> List.foldl (+) 0 -- take the sum

-- no alpha-beta pruning yet
-- evaluates leafs only
evalMoves : Player -> MoveTree -> MoveTree
evalMoves col t =
  case t of
    T _ _ ->
      t
    L gs ->
      if isOver gs /= NotDone then
        L gs -- game is over and needs no more evaluation
      else
        let
          -- make an array for easy roll-value access
          rolls : BT.Tree Int
          rolls = BT.fromList (List.range 1 5) -- 0-indexed
    
          -- Possible TODO: sort pawn list in descending order
          -- for alpha/beta pruning efficiency
          pawnList : List Pawn
          pawnList =
            case gs.turn of
              White ->
                gs.whitePawns
              Black ->
                gs.blackPawns
    
          -- in case there are no moves
          listDefault : a -> List a -> List a
          listDefault def xs =
            case xs of
              [] -> [def]
              _  -> xs
    
          -- for each possible roll, find the possible moves and outcomes
          legalMoveResults : MoveArray
          legalMoveResults =
            rolls              |> BT.map    (\roll ->
            pawnList           |> List.concatMap  (\p    ->
            makeMove p roll gs |> Maybe.map (\js   ->
            [(Just p, L js)]
            ) |> Maybe.withDefault []
            ) |> listDefault (Nothing, L <| switchTurn gs)
            )
        in
          T gs legalMoveResults

childCount : MoveTree -> List Int
childCount t =
  case t of
    L _ ->
      [0]
    T gs moves ->
      moves
        |> BT.map List.length
        |> BT.toList


-- for now don't force anything
getChild : Pawn -> Int -> MoveTree -> Maybe MoveTree
getChild p roll t =
  case t of
    L gs ->
      Nothing
    T gs moves ->
      let
        getFirst : (a -> Bool) -> List a -> Maybe a
        getFirst f xs =
          case xs of
           [] -> Nothing
           x :: rest ->
             if f x
             then Just x
             else getFirst f rest
      in
        BT.getElem roll moves |> Maybe.andThen
        (getFirst (\elem -> Just p == Tuple.first elem)
        >> Maybe.map Tuple.second)

listMovesForRoll : Int -> MoveTree -> List Pawn
listMovesForRoll roll t =
  case t of
    L _ ->
      []
    T gs moves ->
      BT.getElem roll moves |> Maybe.map (\mlist ->
      mlist                 |> List.concatMap (\entry ->
        Tuple.first entry |> Maybe.map (\mp ->
        [mp]
        ) |> Maybe.withDefault []
      )
      ) |> Maybe.withDefault []

-- list of (roll, [pawns])
listMoves : MoveTree -> List (Int, List Int)
listMoves t =
  case t of
    L _ ->
      [(0, [-1])]
    T gs moves ->
      let rolls = (List.range 1 5) in
      rolls                 |> List.map (\roll ->
      ( roll
      , BT.getElem roll moves |> Maybe.map (\mlist ->
        mlist                 |> List.map (\entry ->
          Tuple.first entry |> Maybe.map (\mp ->
          mp.square
          ) |> Maybe.withDefault -1
        )
        ) |> Maybe.withDefault []
      )
      )