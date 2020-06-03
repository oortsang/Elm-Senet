-- Jeffrey Huang and Oliver Tsang, Spring 2020
-- Senet in Elm (Course Project) for CS 223, Functional Programming
-- Board.elm: store the main game information and rules

-- Kendall's Rules:
--   https://www.startwithabook.org/content/pdfs/EgyptianSenetGame.pdf
--   http://www.gamecabinet.com/history/Senet.html

-- File Layout:
-- There are many helper functions, so I list the
-- type definitions and more useful functions:
--   1. Game State
--      - GameState type def
--      - Player type def
--      - GameOutcome type def
--      - isOver (check for completion)
--   2. Square Definitions
--      - Board type def
--      - SquareState type def
--      - SquareType type def
--      - EndSquare type def
--   3. Pawn Definitions
--      * initPawnCount -- can change rules
--      - Pawn type def
--   4. Board String Representation
--      - boardToStrings (string triple)
--      - boardToString (one line)
--      - printBoard (easy printing for command line)
--   5. Board Initialization
--      - initBoard
--      - initGame
--   6. Checking for Legal Moves
--      - isLegal
--   7. Pawn Movement and Game Logic
--      - easyMove (easy for command line interface)
--      * makeMove -- can change turn termination rules
--      * playPawn -- most of the rule functionality lives here

module Board exposing (..)

import BoardTree as BT exposing (..)




------ 1. Game state ------

type alias GameState =
  { turn         : Player
  , whitePawns   : List Pawn
  , blackPawns   : List Pawn
  , whitePawnCnt : Int
  , blackPawnCnt : Int
  , board        : Board
  }

type Player = Black | White

type GameOutcome
  = NotDone
  | Won Player

isOver : GameState -> GameOutcome
isOver gs =
  if gs.whitePawnCnt == 0 then
    Won White
  else if gs.blackPawnCnt == 0 then
    Won Black
  else
    NotDone

-- assume for efficiency that pawn count is not modified here
updateList : Player -> (List Pawn -> List Pawn) -> GameState -> GameState
updateList col f gs =
  case col of
    White ->
      { gs | whitePawns = f gs.whitePawns }
    Black ->
      { gs | blackPawns = f gs.blackPawns }


------ 2. Square definitions ------

type alias Board = BT.Tree SquareState

type SquareState
  = Free
  | Occ Player

-- Keep track of the types of squares
-- Note that it is impossible to leave the board from a regular square
-- (must visit square 25 first)
-- *Note that all square numbers are zero-indexed!

type SquareType
  = Reg               -- most of the squares (regular)
  | Rebirth           -- square 14; (House of Rebirth) - return square
  | Spec EndSquare -- squares 25-29, detailed below

-- Special rules for the last 5 squares
type EndSquare
  = Happy     -- square 25; (House of Happiness) - mandatory for all pawns
  | Water     -- square 26; (House of Water) - sends back to rebirth
  | Truths    -- square 27; (House of Three Truths) - pawn can only leave with a 3
  | Reatoum   -- square 28; (House of Re-Atoum) - pawn can only leave with 2
  | Horus     -- square 29; (House of Horus) - Free to leave

squareType : Int -> SquareType
squareType n =
  case n of
    14 -> Rebirth
    25 -> Spec Happy
    26 -> Spec Water
    27 -> Spec Truths
    28 -> Spec Reatoum
    29 -> Spec Horus
    _  -> Reg


------ 3. Pawn definitions -----

-- Control if we want to start with 5 or 7 pawns per player...
-- Jequier's rules use 5, while Kendall's rules use 7.
-- For testing purposes, fewer may be easier
initPawnCount : Int
initPawnCount = 7

-- Not sure whether we need to track alive/dead or happy/not happy
-- we should just be able to control movement past 26

-- Pawn will remember its square (for convenience)
type alias Pawn = { color: Player, square: Int }

getSquareColor : SquareState -> Maybe Player
getSquareColor p =
  case p of
    Free -> Nothing
    Occ col -> Just col


------ 4. Board String Representation ------
-- Intended for debugging/command-line purposes
boardToStrings : Board -> (String, String, String)
boardToStrings board =
  let
    blankRep : SquareType -> Char
    blankRep sq =
      case sq of
        Reg          -> '.'
        Rebirth      -> ','
        Spec Happy   -> '&'
        Spec Water   -> 'w'
        Spec Truths  -> '3'
        Spec Reatoum -> '2'
        Spec Horus   -> '.'
    pawnRep : Player -> Char
    pawnRep p =
      case p of
        White -> 'P'
        Black -> 'Q'
    rep : Int  -> Char
    rep n =
      case (BT.getElem n board) of
        Nothing      -> '.'
        Just (Free)  -> blankRep (squareType n)
        Just (Occ p) -> pawnRep p
    nums1 = List.range 0 9
    nums2 = List.reverse <| List.range 10 19
    nums3 = List.range 20 29
    space = List.intersperse ' '
    line ns = String.fromList <| space <| List.map rep ns
    _ = Debug.log "Row 1" (line nums1)
    _ = Debug.log "Row 2" (line nums2)
    _ = Debug.log "Row 3" (line nums3)
  in
    (line nums1, line nums2, line nums3)

boardToString : Board -> String
boardToString b =
  let
    (l1, l2, l3) = boardToStrings b
  in
    l1 ++ "\n" ++ l2 ++ "\n" ++ l3

-- debugging purposes
printBoard : Maybe GameState -> Maybe String
printBoard = Maybe.map (\g -> boardToString g.board)


------ 5. Board Initialization ------

-- initial board state
-- white is behind at the moment
initBoard : BT.Tree SquareState
initBoard =
  let
    pawnRange = List.range 0 (2*initPawnCount - 1)
    sorter n =
      if modBy 2 n == 0 then
        White
      else
        Black
    pawns = List.map (\i -> Occ (sorter i)) pawnRange
    squareList = pawns ++ (List.repeat (30-2*initPawnCount) Free)
  in
    BT.fromList squareList

initGame : GameState
initGame =
  { turn = Black
  , whitePawns =
      List.map
        (\i -> { color = White, square = 2*i })
        (List.range 0 (initPawnCount-1))
  , blackPawns =
      List.map
        (\i -> { color = Black, square = 2*i+1 })
        (List.range 0 (initPawnCount-1))
  , whitePawnCnt = initPawnCount
  , blackPawnCnt = initPawnCount
  , board = initBoard
  }



------ 6. Checking for Legal Moves ------

-- check whether a pawn on square square could
-- move to a square of number m
-- Note: m may not lie on the board (so using an Int is most convenient)
legalBySquareType : SquareType -> Int -> Bool
legalBySquareType square m =
  case square of
    Spec Happy ->
      True
    Spec Water ->
      True
    Spec Truths ->
      m == 30 -- need to leave the board exactly here
    Spec Reatoum ->
      m == 30 -- need to leave the board exactly here
    Spec Horus ->
      True
    _ ->
      True

-- can a given pawn move to square m?
isLegal : Board -> Pawn -> Int -> Bool
isLegal board p roll =
  let
    n = p.square
    m = n + roll

    -- Conditionals
    legalSqType =
      legalBySquareType (squareType n) m
    skippedHappiness =
      n < 25 && m > 25
    attemptedLeave =
      m >= 30
  in
    if not legalSqType then
      -- Move is illegal based on current square type
      False
    else if skippedHappiness then
      -- Don't skip happiness day! (sq 25)
      False
    else if attemptedLeave then
      -- Attempts to leave will result in (mdest == Nothing).
      -- We've already checked boundary conditions
      -- in legalBySquareType.
      True
    else
      -- check the destination square
      case BT.getElem m board of
        Nothing ->
          let _ = Debug.log "Yikes, out-of-bounds m not handled..." m in
          False
        Just (Free) ->
          True
        Just (Occ destCol) ->
          -- only possible if the garget is from the other player
          p.color /= destCol

-- Find all moves that are legal for a given die roll
-- and maybe should compute the result of making that move
-- (-> List (Pawn, GameState) output in that case)
-- legalMoves : GameState -> Int -> List (Pawn)




------ 7. Pawn Movement and Game Logic ------
-- Mini table of contents:
--   Helper functions
--     pawnSwapHelper:
--       swap pawns at given squares (as integers)
--     pawnSwap:
--       swap pawns at given squares (as integers)
--       operates on GameState rather than boards
--       *updates the game state to reflect pawn movement
--     clearSquare:
--       remove the pawn from a square
--     removePawn:
--       remove a pawn from the game state and board
--     lastFreeBy:
--       identify the last free square before the requested square
--     switchTurn:
--       flip the turn in the game state
--   Main functions to call
--     easyMove:
--       move a pawn from square n to m
--       easier interface for command-line testing
--     makeMove:
--       Moves desired pawn (if possible)
--       Pawns in squares 27-29 slide back if valid
--       Checks conditions and ends the turn (currently always)
--     playPawn:
--       Moves desired pawn (if possible)
--       Does redirect from house of water to rebirth
--       * technically a helper function of makeMove but does
--         the heavy lifting, so I decided to put it at the end


-- helper function to swap pawn positions
-- should work fine if the destination is empty
pawnSwapHelper :  Int -> Int -> Board -> Maybe Board
pawnSwapHelper pn qn board =
  BT.swap pn qn board


-- helper function to swap pawn positions
-- but also updates the game state
pawnSwap : Int -> Int -> GameState -> Maybe GameState
pawnSwap pn qn gs =
  case BT.getElem qn gs.board of
    Nothing ->
      Nothing
    Just q ->
      let
        -- replace the square value
        replaceHelper oldv newv x =
          if x.square == oldv
          then { color = x.color, square = newv }
          else x
        replace old new =
          List.map (replaceHelper old new)

        -- components
        maybeBoard = pawnSwapHelper pn qn gs.board
        js =
          case Maybe.andThen getSquareColor (getElem pn gs.board) of
            Just col ->
              updateList col (replace pn qn) gs
            Nothing ->
              let _ = Debug.log "Oops, invalid pawnSwap?" () in
              gs
        hs = Maybe.map (\b -> { js | board = b }) maybeBoard
        -- _ = Debug.log "New Board" (BT.toList js.board)
      in
        case q of
          Free ->
            hs
          Occ destCol ->
            -- does not enforce different colors
            Maybe.map
              (\game ->  updateList destCol (replace qn pn) game)
              hs


-- clears the specified square on the board
clearSquare : Board -> Int -> Board
clearSquare board n =
  Maybe.withDefault board
    <| BT.setElem n Free board

-- remove a pawn from the game state record and board
removePawn : Pawn -> GameState -> GameState
removePawn p gs =
  let
    newBoard = clearSquare gs.board p.square
    removeListElem x =
      List.filter ((/=) x)
  in
    case p.color of
      White ->
        { gs
        | whitePawnCnt = gs.whitePawnCnt - 1
        , whitePawns   = removeListElem p gs.whitePawns
        , board = newBoard
        }
      Black ->
        { gs
        | blackPawnCnt = gs.blackPawnCnt - 1
        , blackPawns   = removeListElem p gs.blackPawns
        , board = newBoard
        }

-- help find an open spot for a pawn moving to the house of rebirth
lastFreeBy : Int -> GameState -> Maybe Int
lastFreeBy sq gs =
  case BT.getElem sq gs.board of
    Nothing   -> Nothing
    Just Free -> Just sq
    _         -> lastFreeBy (sq-1) gs

-- helper to switch the turn to the opposite player
switchTurn : GameState -> GameState
switchTurn gs =
  case gs.turn of
    White ->
      { gs | turn = Black }
    Black ->
      { gs | turn = White }

-- for command-line purposes
easyMove : Int -> Int -> Maybe GameState -> Maybe GameState
easyMove n m mgs =
  -- Maybe.map switchTurn <| -- happens in makeMove
  Maybe.andThen
    (\gs ->
       makeMove {color=gs.turn, square=n} (m-n) gs)
       -- playPawn {color=gs.turn, square=n} (m-n) gs)
    mgs

-- Makes the move for one pawn and takes care of pawns
-- on squares 27-29 sliding back to rebirth (if they were
-- not given a roll that would let them leave the board)
makeMove : Pawn -> Int -> GameState -> Maybe GameState
makeMove p roll gs =
  let
    -- Decide whether to end the turn
    -- Could check the roll to decide whether to switch turns
    -- (currently skipped but left as an option)
    -- At the moment, I think evaluating later makes more sense
    -- (hence the lazy eval)
    endOrContinueTurn : () -> GameState -> GameState
    endOrContinueTurn =
      \() ->
        switchTurn

    -- Check for pawns in the end zone and send them back.
    -- checkSquare: compares square sq in gs and js,
    -- and if a pawn with the same color as p has stayed
    -- in place, it gets kicked back to rebirth
    checkSquare : Int -> GameState -> GameState
    checkSquare sq js =
      let getsq g = BT.getElem sq g.board in
      case (getsq gs, getsq js) of
        (Just (Occ c1), Just (Occ c2)) ->
          if (c1==c2) && (c1 == p.color) then
            -- boot pawn on square sq to the
            -- the last open spot before rebirth
            lastFreeBy 14 js |>
              Maybe.andThen (\lf ->
              pawnSwap sq lf js) |>
              Maybe.withDefault js
          else
            js
        _ ->
          js
    sendBack : GameState -> GameState
    sendBack js =
      js
        |> checkSquare 27
        |> checkSquare 28
        |> checkSquare 29
  in
    -- Logic order:
    --   1. Let the roll play out (playPawn)
    --   2. Demote pawns that were originally in the end zone
    --      (pawns of the current player) that have not left
    --   3. End the turn (maybe check the roll first)
    gs
      |> playPawn p roll
      |> Maybe.map sendBack
      |> Maybe.map (endOrContinueTurn ())


-- Moves one pawn and updates the game state
-- Handle elsewhere: squares 27-29 sliding back to house of water
-- Note: The motivation behind taking p as a pawn rather than the
--       an int is that the AI would have easy access to pawn data
--       and makes more requests than the human user
playPawn : Pawn -> Int -> GameState -> Maybe GameState
playPawn p roll gs =
  let
    n = p.square
    m = n + roll

    -- Conditionals
    legalSqType =
      legalBySquareType (squareType n) m
    skippedHappiness =
      n < 25 && m > 25
    attemptedLeave =
      m >= 30

    -- Helper functions
    -- moveTo square m, which has state dest
    moveTo : SquareState -> Maybe GameState
    moveTo destState =
      case destState of
        Free ->
          -- swap pawn on n with empty m
          pawnSwap n m gs
        Occ destCol ->
          -- swap with enemy pawn on m
          if p.color /= destCol then
            pawnSwap n m gs
          else
            let _ = Debug.log "same color! (p, destCol)" (p, destCol) in
            Nothing
    -- Move to rebirth square
    moveToRebirth : () -> Maybe GameState
    moveToRebirth =
      \() ->
        -- last unoccupied square before 14
        (lastFreeBy 14 gs)  |>
          Maybe.andThen (\d ->
          pawnSwap n d gs)
  in
    -- Check if it's your turn
    if p.color /= gs.turn then
      let
        _ = Debug.log
          "Getting lost in the moves... (pawn vs. turn)"
          (p, gs.turn)
      in
      Nothing
    -- If destination is illegal just by square type
    -- meant for checking the last few squares
    -- send back to rebirth
    else if not legalSqType then
      moveToRebirth ()
    -- Enforce landing on house of happiness
    else if skippedHappiness then
      let _ = Debug.log "skippedHappiness" skippedHappiness in
      Nothing
    -- Let the pawn leave the board
    else if attemptedLeave then
      -- attempt to leave board is legitimate since we checked earlier
      Just (removePawn p gs)
    -- Check for collisions and swap
    else
      case (BT.getElem m gs.board) of
        Nothing ->
          let _ = Debug.log "Yikes, out-of-bounds m not handled..." m in
          Nothing
        Just dest ->
          -- decide whether to handle sliding back here or not...
          case squareType m of
            Spec Water ->
              -- moveTo dest
              moveToRebirth ()
            _ ->
              moveTo dest
