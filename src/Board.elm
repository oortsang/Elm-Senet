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

-- assume for efficiency that pawn count is not modified here
updateList : Player -> (List Pawn -> List Pawn) -> GameState -> GameState
updateList col f gs =
  case col of
    White ->
      { gs | whitePawns = f gs.whitePawns }
    Black ->
      { gs | blackPawns = f gs.blackPawns }


------ Square definitions ------

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

getSquare : Int -> GameState -> Maybe SquareState
getSquare i gs =
  BT.getElem i gs.board


------ Pawn definitions -----

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


------ Board String Representation ------
-- Intended for debugging/command-line purposes
boardToString : Board -> String
boardToString board =
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
    (line nums1) ++ "\n" ++ (line nums2) ++ "\n" ++ (line nums3)

-- debugging purposes
printBoard : Maybe GameState -> Maybe String
printBoard = Maybe.map (\g -> boardToString g.board)


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



------ Checking for legal moves ------

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




------ Pawn movement ------

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
easyMove n m =
  Maybe.andThen
   (\gs ->
     makeMove {color=gs.turn, square=n} (m-n) gs)

-- makes the move and updates the game state
-- Handle elsewhere: House of water backward behavior
-- Handle elsewhere: squares 27-29 sliding back to house of water
makeMove : Pawn -> Int -> GameState -> Maybe GameState
makeMove p roll gs =
  let
    n = p.square
    m = n + roll
    -- in some versions rolls of 1,4,5 let you play again
    js = switchTurn gs

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
          pawnSwap n m js
        Occ destCol ->
          -- swap with enemy pawn on m
          if p.color /= destCol then
            pawnSwap n m js
          else
            let _ = Debug.log "same color! (p, destCol)" (p, destCol) in
            Nothing
    -- Move to rebirth square
    moveToRebirth : () -> Maybe GameState
    moveToRebirth =
      let
        lastFreeBy : Int -> Maybe Int
        lastFreeBy sq =
          case BT.getElem sq js.board of
            Nothing   -> Nothing
            Just Free -> Just sq
            _         -> lastFreeBy (sq-1)
        -- last unoccupied square not after house of water
        dest = lastFreeBy 14
      in
        \() ->
          Maybe.andThen (\d -> pawnSwap n d js) dest
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
      let _ = Debug.log "legalSqType" legalSqType in
      -- Nothing -- <- if we don't want sliding back
      moveToRebirth ()  -- <- if we want sliding back
    -- Enforce landing on house of happiness
    else if skippedHappiness then
      let _ = Debug.log "skippedHappiness" skippedHappiness in
      Nothing
    -- Let the pawn leave the board
    else if attemptedLeave then
      -- attempt to leave board is legitimate since we checked earlier
      Just (removePawn p js)
    -- Check for collisions and swap
    else
      -- TODO: Check (squareType m) for house of water
      -- Check destination square (js for switched turn)
      case (BT.getElem m js.board) of
        Nothing ->
          let _ = Debug.log "Yikes, out-of-bounds m not handled..." m in
          Nothing
        Just dest ->
          -- decide whether to handle sliding back here or not...
          case squareType m of
            Spec Water ->
              -- moveTo dest
              moveToRebirth () -- house of rebirth
            _ ->
              moveTo dest
