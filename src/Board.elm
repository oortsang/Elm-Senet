-- Jeffrey Huang and Oliver Tsang, Spring 2020
-- Senet in Elm (Course Project) for CS 223, Functional Programming
-- Board.elm: store the main game information

-- Kendall's Rules:
--   https://www.startwithabook.org/content/pdfs/EgyptianSenetGame.pdf
--   http://www.gamecabinet.com/history/Senet.html

module Board exposing (..)

import BoardTree as BT exposing (..)




------ Game state ------

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



------ Square definitions ------

type alias Board = BT.Tree SquareState

type SquareState
  = Free
  | Occ Pawn

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

getSquare : Int -> GameState -> Maybe SquareState
getSquare i gs =
  BT.getElem i gs.board


------ Pawn information -----

-- Control if we want to start with 5 or 7 pawns per player...
-- Jequier's rules use 5, while Kendall's rules use 7.
-- For testing purposes, fewer may be easier
initPawnCount : Int
initPawnCount = 7

-- Not sure whether we need to track alive/dead or happy/not happy
-- we should just be able to control movement past 26

-- Pawn will remember its square (for convenience)
type alias Pawn = { color: Player, square: Int }





------ Board Initialization ------

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
    pawns = List.map (\i -> Occ {color = sorter i, square = i}) pawnRange
    squareList = pawns ++ (List.repeat (30-2*initPawnCount) Free)
  in
    BT.fromList squareList



------ Checking for legal moves ------

-- can a given pawn move to square m?
isLegal : Board -> Pawn -> Int -> Bool
isLegal board p m =
  let
    n = p.square
    currSquare = squareType n
    mdest = BT.getElem m board
    skippedHappiness =
      n < 25 && m > 25
    attemptedLeave =
      m >= 30
    legalBySquareType =
      case currSquare of
        Spec Happy ->
          True
        Spec Water ->
          True
        Spec Truths ->
          m == 30 -- need to leave exactly here
        Spec Reatoum ->
          m == 30 -- need to leave exactly here
        Spec Horus ->
          True
        _ ->
          True
  in
    if not legalBySquareType then
      -- Move is illegal based on current square type
      False
    else if skippedHappiness then
      -- Don't skip happiness day!
      False
    else if attemptedLeave then
      -- Attempts to leave will result in (mdest == Nothing).
      -- We've already checked boundary conditions
      -- in legalBySquareType.
      True
    else
      -- check the destination square
      case mdest of
        Nothing ->
          let _ = Debug.log "Yikes, out-of-bounds m not handled..." m in
          False
        Just (Free) ->
          True
        Just (Occ q) ->
          -- only possible if the garget is from the other player
          p.color /= q.color
