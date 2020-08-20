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
      -- _ = Debug.log "Row 1" l1
      -- _ = Debug.log "Row 2" l2
      -- _ = Debug.log "Row 3" l3
    in
      l1 ++ "\n" ++ l2 ++ "\n" ++ l3
  )

------ Minimax ------
-- TODO:
--   1. Make lazy !
--   2. Improve heuristic function


-- score roughly represents the (opposite of) distance that black
-- needs to move its pieces compared to white
-- where higher is better for black
gsLeafVal : Player -> GameState -> Float
gsLeafVal col gs =
  let
    colCorrection =
      toFloat <|
        case col of
          Black ->  1
          White -> -1

    -- arbitrarily set black to negative
    -- so that it evaluates from black's perspective
    sign sq =
      toFloat <|
        case sq of
          Free      ->  0
          Occ Black -> -1
          Occ White ->  1

    rebirthSquare =
      (lastFreeBy 14 gs)
      |> Maybe.withDefault 0

    -- extra reward for having promoted each pawn
    pawnBounty = toFloat 20

    pawnAdv =
      pawnBounty * toFloat (gs.whitePawnCnt - gs.blackPawnCnt)

    -- Value Function
    -- Choose values based on distance from the end
    valf : Int -> SquareState -> Float
    valf i sq =
      case squareType i of
        Spec Happy ->
          (sign sq) * -3.0 -- want to provide an extra incentive to go here
        Spec Horus ->
          -- basically as good as having promoted it
          (sign sq) * -pawnBounty
        Spec Reatoum ->
          -- 1/4 chance of success
          -- 3/4 chance of demotion
          (sign sq) * ( 0.75 * (toFloat (30 - rebirthSquare))
                      - 0.25 * pawnBounty)
        Spec Truths ->
          -- 3/8 chance of success
          -- 5/8 chance of demotion
          (sign sq) * ( 0.625 * (toFloat (30 - rebirthSquare))
                      - 0.375 * pawnBounty)
        _ ->
          if i /= 24 then
            (sign sq) * (toFloat (30-i))
          else
            (sign sq) * 8.5 -- make it worse than square 22
  in
    (*) colCorrection <|
    case isOver gs of
      NotDone ->
        -- maybe could make more efficient with some
        -- kind of enumerate-toList/map type thing
        List.range 0 29        |> List.map  (\i ->
        BT.getElem i gs.board  |> Maybe.map (\sq ->
        (valf i sq) + (pawnAdv)
        ) |> Maybe.withDefault 0.0)
        |> List.foldl (+) 0.0 -- take the sum
      Won White ->
        -1000.0
      Won Black ->
        1000.0

-- Wrapper for gsLeafVal to apply to thunkstate
tsLeafVal : Player -> ThunkState -> Float
tsLeafVal col (N gs tmt) =
  gsLeafVal col gs


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
      pawnList           |> List.concatMap  (\p ->
      makeMove p roll gs |> Maybe.map (\js ->
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

-- like playing a move
-- but the play may not be valid (or the game may end...)
getChild : Maybe Pawn -> Int -> ThunkState -> Maybe ThunkState
getChild mp roll (N gs tmt) =
  let
    -- _ = Debug.log "getChild called with (mp, roll)" (mp, roll)
    -- _ = Debug.log "current turn:" gs.turn
    ma =
      case tmt of
        Lazy tma -> force tma
        Eval rma -> rma
    mlist = BT.getElem (roll-1) ma
    getpawn xs =
      case xs of
        [] ->
          Nothing
        (xp, ts) :: rest ->
          if mp == xp
          then Just ts
          else getpawn rest
  in
    -- mlist |> Maybe.andThen (\list ->
    -- getpawn list)
    Maybe.andThen getpawn mlist

mmArg : Bool -> (a -> comparable) -> List a -> Maybe a
mmArg isMax f ys =
  let
    minormax : comparable -> comparable -> comparable
    minormax =
      if isMax
      then max
      else min
    mmaHelper : a -> comparable -> List a -> a
    mmaHelper acc facc xs =
      case xs of
        x :: rest ->
          let fx = (f x) in
          if fx == minormax facc fx then
            mmaHelper x fx rest
          else
            mmaHelper acc facc rest
        _ ->
          acc
  in
    case ys of
      [] -> Nothing
      y :: rest ->
        Just <| mmaHelper y (f y) rest

-- evaluate the state at a given ply, and return the thunkstate to encourage laziness
-- currently not updating the state properly (just delayed evaluation)
evalState : Player -> Int -> ThunkState -> Maybe (Float, ThunkState)
evalState col ply (N gs tmt) =
  if ply == 0 then
    -- evaluate the current state without further evaluation
    Just <| (gsLeafVal col gs, N gs tmt)
  else
    -- evaluate each of the possibilities of the children...
    -- then take the best move
    let
      mmArgCol = mmArg (col == gs.turn)
      -- minimax =
      --   mmArgCol Tuple.first
      expect : List (Maybe (Float, a)) -> Maybe Float
      expect =
        let
          expHelper : Float -> List Float -> List (Maybe (Float, a)) -> Maybe Float
          expHelper acc weights moves =
            case (weights, moves) of
              (w :: wrest, m :: mrest) ->
                m |> Maybe.andThen (\(val, _) ->
                expHelper (acc+w*val) wrest mrest)
              ([], []) ->
                Just acc
              _ ->
                -- let _ = Debug.log "Problem taking expectation..." in
                Nothing
        in
          expHelper 0 [1/16, 1/4, 3/8, 1/4, 1/16]

      ma = evalTMT tmt
    in
      case isOver gs of
        Won winner ->
          Just <|
            ( if winner == col
              then  255
              else -255
            , N gs (Eval ma))
        NotDone ->
          -- for each possible roll,
          -- get the list from the MoveArray ma
          -- then get element tup : (Maybe Pawn, ThunkState)
          -- get the next thunkstate
          let
            -- _ = Debug.log "options" preList
            -- _ = Debug.log "depth" (evalDepth <| N gs <| Eval updatedMA)
            preList =
              List.range 1 5         |> List.map (\roll ->
              BT.getElem (roll-1) ma |> Maybe.andThen (\list ->
              let
                -- fetch move values and future thunkstates
                tupList =
                  list                     |> List.map  (\(mp, ts) ->
                  evalState col (ply-1) ts |> Maybe.map (\(moveVal, newTS) ->
                  (moveVal, (mp, newTS))
                  )
                  ) |> mlistConcat

                -- track legal future mp/thunkstate pairs
                movesForRoll =
                  List.map Tuple.second tupList
              in
                -- tupList |> mmArgCol Tuple.first
                tupList |> mmArgCol Tuple.first |> Maybe.map (\mval ->
                  (mval, movesForRoll)
                )
              )
              )
            -- move array that integrates the new results
            updatedMA =
                preList
                  |> List.map (Maybe.map Tuple.second)
                  |> mlistConcat
                  |> BT.fromList
          in
            preList
              |> List.map (Maybe.map Tuple.first)
              |> expect
              |> Maybe.map (\vf ->
                (vf, N gs <| Eval updatedMA)
                -- (vf, N gs <| Eval ma) -- discard newly computed changes
              )


-- helper function for evalState and handling the list of maybes
mlistConcat : List (Maybe a) -> List a
mlistConcat xs =
  xs |> List.concatMap (\mx ->
    case mx of
      Nothing -> []
      Just x  -> [x]
  )


-- like evalState but assumes the given roll to save on computation
evalRolledState : Player -> Int -> Int -> ThunkState -> Maybe (Float, Maybe Pawn, ThunkState)
evalRolledState col ply roll (N gs tmt) =
  if ply == 0 then
    Just <| (gsLeafVal col gs, Nothing, N gs tmt)
  else
    -- evaluate each of the possibilities of the children...
    -- then take the best move
    let
      mmCol = mmArg (col == gs.turn) (\w->w)
      mmArgColFirst = mmArg (col == gs.turn) Tuple.first
      ma = evalTMT tmt
    in
      case isOver gs of
        Won winner ->
          Just <|
            ( if winner == col
                then  255
                else -255
            , Nothing
            , N gs (Eval ma))
        NotDone ->
          BT.getElem (roll-1) ma |> Maybe.andThen (\list ->
          let
            preList =
              list                     |> List.filterMap (\(mp, ts) ->
              evalState col (ply-1) ts |> Maybe.map (\(val, newTS) ->
              -- let _ = Debug.log "tmpdepth" (evalDepth newTS) in
              (val, (mp, newTS))
              )
              )

            mmElem =
              preList |> mmArgColFirst

            -- extract move array info
            updatedMoves =
              preList
                |> List.map Tuple.second


            optionDescription =
              preList |> List.map (\(v, (mp, newts)) ->
              (mp, v)
              )
          in
            mmElem |> Maybe.map (\elem ->
              ( elem |> Tuple.first
              , elem |> Tuple.second |> Tuple.first
              , let
                  newestTS =
                    BT.setElem (roll-1) updatedMoves ma
                      |> Maybe.withDefault ma
                      |> Eval
                      |> N gs
                in
                  newestTS
              )
            )

          )

-- Wrapper that just returns the pawn and thunkstate
aiChooseMove : Player -> Int -> Int -> ThunkState -> (Maybe Pawn, ThunkState)
aiChooseMove col ply roll ts =
  let
    res = evalRolledState col ply roll ts
  in
    ( Maybe.andThen (\(_, mb, _) -> mb) res
    , Maybe.map (\(_,_,newTS)->newTS) res
        |> Maybe.withDefault ts
    )

-- tells how many levels have been evaluated for a given thunkstate
evalDepth : ThunkState -> Int
evalDepth (N gs tmt) =
  let
    edTail n tmt2 =
      case tmt2 of
        Lazy _ ->
          n
        Eval ma ->
          BT.getElem 0 ma |> Maybe.andThen (\list ->
          List.head list  |> Maybe.map (\(_, (N _ tmt3)) ->
          edTail (n+1) tmt3
          )
          ) |> Maybe.withDefault n
  in
    edTail 0 tmt


-- Alternate easier heuristic AI
-- always plays the last piece
lastPawnAI : Player -> Int -> ThunkState -> (Maybe Pawn, ThunkState)
lastPawnAI col roll (N gs tmt) =
  let
    mgt : Maybe Pawn -> Maybe Pawn -> Bool
    mgt mx my =
      case (mx, my) of
        (Just x, Just y) ->
          x.square > y.square
        (Just x, Nothing) ->
          True
        (Nothing, Just y) ->
          False
        (Nothing, Nothing) ->
          -- this case won't come up anyway...
          False
    lastPawnHelper (max, maxTS) xs =
      case xs of
        (mx, xTS) :: rest ->
          if mgt mx max then
            lastPawnHelper (mx, xTS) rest
          else
            lastPawnHelper (max, maxTS) rest
        _ ->
          (max, maxTS)

    lastPawn : List (Maybe Pawn, ThunkState) -> Maybe (Maybe Pawn, ThunkState)
    lastPawn xs =
      case xs of
        (mx, newTS) :: [] ->
          Just (mx, newTS)
        (mx, newTS) :: rest ->
          Just <| lastPawnHelper (mx, newTS) rest
        _ ->
          Nothing
    ma = evalTMT tmt
  in
    BT.getElem (roll-1) ma |> Maybe.andThen (\moveList ->
    lastPawn moveList
    -- endOf moveList -- in the form (mp, ts)
    ) |> Maybe.withDefault (Nothing, newNode (skipTurn gs)) -- is this right...