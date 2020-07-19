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

type Thunk a = Thunk (() -> a)

force : Thunk a -> a
force (Thunk f) = f ()

lazy : (() -> a) -> Thunk a
lazy = Thunk

------ Move tree definitions ------
-- eager for now
-- also missing the value for now
-- value will be evaluated lazily later...
type MoveTree -- comparable
  -- = L comparable GameState -- leaf
  = L GameState
  | P
      -- comparable -- value
      GameState  -- current gs
      MoveArray  -- next moves



type alias MoveArray =
  BT.Tree (List (Maybe Pawn, ThunkState))

-- Get around the lack of memoization
type ThunkMoveTree
  = Lazy (Thunk MoveArray)
  | Eval MoveArray  -- already evaluated

type ThunkState =
  N GameState ThunkMoveTree

-- Wraps a gamestate in a thunkstate with
-- future states being lazily evaluated
tsWrapper : GameState -> ThunkState
tsWrapper gs =
  N gs <| Lazy <| lazy <| \() -> findMoves gs


-- print helper
printTSBoard : Maybe ThunkState -> Maybe String
printTSBoard =
  Maybe.map (\(N g _) ->
    let
      (l1, l2, l3) = boardToStrings g.board
      _ = Debug.log "Row 1" l1
      _ = Debug.log "Row 2" l2
      _ = Debug.log "Row 3" l3
    in
      l1 ++ "\n" ++ l2 ++ "\n" ++ l3
  )

------ Minimax ------
-- TODO:
--   1. Handle game termination situation
--   2. Control how many steps to look forward
--   3. Handle case where there are no legal moves so you need to skip forward...
--   4. Make lazy !



-- 0 is the best possible score for black
-- since it means black has promoted everything
gsLeafVal : GameState -> Int
gsLeafVal gs =
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
    case isOver gs of
      NotDone ->
        -- maybe could make more efficient with some
        -- kind of enumerate-toList/map type thing
        List.range 0 29        |> List.map  (\i ->
        BT.getElem i gs.board  |> Maybe.map (\sq ->
        valf i sq
        ) |> Maybe.withDefault 0)
        |> List.foldl (+) 0 -- take the sum
      Won White ->
        -100
      Won Black ->
        100

-- Wrapper for gsLeafVal to apply to thunkstate
tsLeafVal : ThunkState -> Int
tsLeafVal (N gs tmt) =
  gsLeafVal gs


-- For each possible roll, find the possible moves and outcomes
findMoves : GameState -> MoveArray
findMoves gs =
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
    -- _ = Debug.log "Finding moves..." pawnList
    -- _ = printBoard (Just gs)

    -- in case there are no moves
    listDefault : a -> List a -> List a
    listDefault def xs =
      case xs of
        [] -> [def]
        _  -> xs
  in
    if isOver gs /= NotDone then
      -- if game is over
      BT.fromList (List.repeat 5 [])
    else
      -- keep going
      rolls              |> BT.map    (\roll ->
      pawnList           |> List.concatMap  (\p    ->
      makeMove p roll gs |> Maybe.map (\js   ->
      [(Just p, tsWrapper js)]
      ) |> Maybe.withDefault []
      ) |> listDefault (Nothing, tsWrapper <| switchTurn gs)
      )


-- Wrap a gamestate as a new node
newNode : GameState -> ThunkState
newNode gs =
  N gs (Lazy <| lazy <| \() -> findMoves gs)

-- Explore new moves
evalNode : ThunkState -> ThunkState
evalNode (N gs tmt) =
  case tmt of
    Lazy tma ->
      N gs (Eval <| force tma)
    Eval ma ->
      -- do nothing
      N gs tmt

evalTMT : ThunkMoveTree -> MoveArray
evalTMT tmt =
  case tmt of
    Lazy tma -> force tma
    Eval rma -> rma

-- Functions to do
listMovesForRoll : Int -> ThunkState -> List Pawn
listMovesForRoll roll (N _ tmt) =
  let
    ma = evalTMT tmt
  in
    BT.getElem (roll-1) ma |> Maybe.map (\a ->
    a                      |> List.concatMap (\elem ->
    case elem of
      (Nothing, _) -> []
      (Just p, _)  -> [p]
    )) |> Maybe.withDefault []


childCount : ThunkState -> Int
childCount (N _ tmt) =
  let
    ma = evalTMT tmt
  in
    ma |> BT.toList
       |> List.map (\xs ->
    xs |> List.map (\x ->
    case x of
      (Nothing, _)     -> 0
      (Just p, newma)  -> 1
    ) |> List.sum
    ) |> List.sum
    -- Debug.todo "count up the number of legal states..."

-- like playing a move
-- but the play may not be valid (or the game may end...)
getChild : Maybe Pawn -> Int -> ThunkState -> Maybe ThunkState
getChild mp roll (N gs tmt) =
  let
    _ = Debug.log "getChild called with (mp, roll)" (mp, roll)
    _ = Debug.log "current turn:" gs.turn
    ma =
      case tmt of
        Lazy tma -> force tma
        Eval rma -> rma
    mlist = BT.getElem (roll-1) ma
    getpawn xs =
      case xs of
        [] ->
          let _=Debug.log "alas" () in
          Nothing
        (xp, ts) :: rest ->
          let _=Debug.log "(mp, xp)" (mp, xp) in
          if mp == xp
          then Just ts
          else getpawn rest
  in
    -- mlist |> Maybe.andThen (\list ->
    -- getpawn list)
    Maybe.andThen getpawn mlist

evalState : Player -> Int -> ThunkState -> Maybe Int
evalState col ply (N gs tmt) =
  if ply == 0 then
    -- evaluate the current state without further evaluation
    -- tsLeafEval ts
    Just <| gsLeafVal gs
  else
    -- evaluate each of the possibilities of the children...
    -- then take the best move
    let
      minimax : List comparable -> Maybe comparable
      minimax =
        if col == gs.turn
        then List.maximum
        else List.minimum
      ma = evalTMT tmt
    in
    case isOver gs of
      Won winner ->
        Just <|
          if winner == col
          then  255
          else -255
      NotDone ->
        -- for each possible roll,
        -- get the list from the MoveArray ma
        -- then get element tup : (Maybe Pawn, ThunkState)
        -- get the next thunkstate
        let
          -- _ = Debug.log "options" preList
          preList =
            (List.range 1 5)    |> List.map (\roll ->
            BT.getElem (roll-1) ma  |> Maybe.andThen (\list ->

            -- let _=Debug.log "list size" (List.length list) in
            list                    |> List.map (\(mp, ts) ->
            evalState col (ply-1) ts
            -- don't need to fetch child with getChild mp roll ts

            ) |> mlistConcat
              |> minimax
            )
            ) |> mlistConcat
        in
          minimax preList
          -- |> minimax

-- helper function for evalState and handling the list of maybes
mlistConcat : List (Maybe a) -> List a
mlistConcat xs =
  xs |> List.concatMap (\mx ->
    case mx of
      Nothing -> []
      Just x -> [x]
  )


-- like evalState but assumes the given roll to save on computation
evalRolledState : Player -> Int -> Int -> ThunkState -> Maybe (Int, Maybe Pawn)
evalRolledState col ply roll (N gs tmt) =
  if ply == 0 then
    Just <| (gsLeafVal gs, Nothing)
  else
    -- evaluate each of the possibilities of the children...
    -- then take the best move
    let
      minormax =
        if col == gs.turn
        then max
        else min
      minimax : List (comparable, a) -> Maybe (comparable, a)
      minimax ys =
        let
          minimaxarg acc xs =
            case (acc, xs) of
              ((va, pa), (vx, px) :: rest) ->
                if vx == minormax vx va then
                  minimaxarg (vx, px) rest
                else
                  minimaxarg (va, pa) rest
              _ ->
                acc
        in
          case ys of
            [] -> Nothing
            y :: rest ->
              Just <| minimaxarg y rest

      ma = evalTMT tmt
    in
    case isOver gs of
      Won winner ->
        Just <|
          ( if winner == col
              then  255
              else -255
          , Nothing)
      NotDone ->
        BT.getElem (roll-1) ma |> Maybe.andThen (\list ->
        let
          _ = Debug.log "" (minimax liszt)
          _ = Debug.log "number of moves" (List.length list)
          liszt =
            list |> List.filterMap (\(mp, ts) ->
            case evalState col (ply-1) ts of
              Just val -> Just (val, mp)
              Nothing  -> Nothing
            )
        in
          liszt |> minimax -- |> Maybe.map Tuple.first
        )

aiChooseMove : Player -> Int -> Int -> ThunkState -> Maybe Pawn
aiChooseMove col ply roll ts =
  let
    res = evalRolledState col ply roll ts
  in
    Maybe.andThen Tuple.second res

-- selectMove : Pawn -> Int -> ThunkState -> ThunkState
