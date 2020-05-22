-- Jeffrey Huang and Oliver Tsang, Spring 2020
-- Senet in Elm (Course Project) for CS 223, Functional Programming
-- Board.elm: store the main game information

-- Kendall's Rules:
--   https://www.startwithabook.org/content/pdfs/EgyptianSenetGame.pdf
--   http://www.gamecabinet.com/history/Senet.html

module Board exposing (..)

import BoardTree as BT exposing (..)


type Player = Black | White


------ Square definitions ------

type Square = Square SquareType SquareState

-- Keep track of the types of squares
-- Note that it is impossible to leave the board from a regular square
-- (must visit square 26 first)
type SquareType
  = Reg               -- most of the squares (regular)
  | Rebirth           -- square 15; (House of Rebirth) - return square
  | Spec EndSquare -- squares 26-30, detailed below

-- Special rules for the last 5 squares
type EndSquare
  = Happy     -- square 26; (House of Happiness) - mandatory for all pawns
  | Water     -- square 27; (House of Water) - sends back to rebirth
  | Truths    -- square 28; (House of Three Truths) - pawn can only leave with a 3
  | Reatoum   -- square 29; (House of Re-Atoum) - pawn can only leave with 2
  | Horus     -- square 30; (House of Horus) - Free to leave

type SquareState
  = Free
  | Occ Pawn

type alias Board = List SquareState




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




------ Game state ------
type GameOutcome
  = NotDone
  | Won Player


type alias GameState =
  { turn : Player
  , whitePawns : List Pawn
  , blackPawns : List Pawn
  , whitePawnCnt : Int
  , blackPawnCnt : Int
  , board : Board
  }

isOver : GameState -> GameOutcome
isOver gs =
  if gs.whitePawnCnt == 0 then
    Won White
  else if gs.blackPawnCnt == 0 then
    Won Black
  else
    NotDone

------ Board Initialization ------

squareType : Int -> SquareType
squareType n =
  case (n+1) of
    15 -> Rebirth
    26 -> Spec Happy
    27 -> Spec Water
    28 -> Spec Truths
    29 -> Spec Reatoum
    30 -> Spec Horus
    _  -> Reg

-- initial board state
-- white is behind at the moment
board : BT.Tree SquareState
board =
  let
    pawnRange = List.range 1 (2*initPawnCount - 1)
    sorter n =
      if modBy 2 n == 0 then
        White
      else
        Black
    pawns = List.map (\i -> Occ {color = sorter i, square = i}) pawnRange
    squareList = pawns ++ (List.repeat (31-2*initPawnCount) Free)
  in
    BT.fromList squareList