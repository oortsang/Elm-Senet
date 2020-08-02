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
--      - getPawn (for external use)
--   4. Board String Representation
--      - boardToStrings (string triple)
--      - boardToString (one line)
--      - printBoard (easy printing for command line)
--   5. Board Initialization
--      - initBoard
--      - initGame


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
initPawnCount = 5

-- Not sure whether we need to track alive/dead or happy/not happy
-- we should just be able to control movement past 26

-- Pawn will remember its square (for convenience)
type alias Pawn = { color: Player, square: Int }

-- for outside files
getPawn : Int -> GameState -> Maybe Pawn
getPawn i gs =
  let
    squarePawn : SquareState -> Maybe Pawn
    squarePawn sq =
      case sq of
        Free -> Nothing
        Occ c -> Just { square=i, color=c }
  in
    BT.getElem i gs.board
      |> Maybe.andThen squarePawn

getSquareColor : SquareState -> Maybe Player
getSquareColor p =
  case p of
    Free -> Nothing
    Occ col -> Just col


------ 4. Board String Representation ------
-- Intended for debugging/command-line purposes
squareToChar : Int -> Maybe SquareState -> Char
squareToChar i s =
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
  in
    case s of
      Just (Occ p) -> pawnRep p
      Just (Free)  -> blankRep (squareType i)
      Nothing      -> '.'


boardToStrings : Board -> (String, String, String)
boardToStrings board =
  let
    rep n =
      squareToChar n (BT.getElem n board)
    nums1 = List.range 0 9
    nums2 = List.reverse <| List.range 10 19
    nums3 = List.range 20 29
    space = List.intersperse ' '
    line ns = String.fromList <| space <| List.map rep ns
    -- Only needed for command line:
    -- _ = Debug.log "Row 1" (line nums1)
    -- _ = Debug.log "Row 2" (line nums2)
    -- _ = Debug.log "Row 3" (line nums3)
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
printBoard = Maybe.map (\g ->
  let
    (l1, l2, l3) = boardToStrings g.board
    _ = Debug.log "Row 1" l1
    _ = Debug.log "Row 2" l2
    _ = Debug.log "Row 3" l3
  in
    l1 ++ "\n" ++ l2 ++ "\n" ++ l3
  )


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
