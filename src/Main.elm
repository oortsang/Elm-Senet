-- Jeffrey Huang and Oliver Tsang, Spring 2020
-- Senet in Elm (Course Project) for CS 223, Functional Programming
-- Main.elm: makes the html page and handles interactions

-- TODO:
--   1. Save computation of thunkstates!


port module Main exposing (..)


-- From our project
import BoardTree as BT exposing (..)
import Board  exposing (..)
import Logic  exposing (..)
import AI     exposing (..)
import Sticks exposing (..)
import Docs   exposing (..)

-- For html-side
import Browser
import Browser.Events

import Html exposing (Html, text, button, div, br, h3)
import Html.Attributes as HA
import Html.Events as HE

import Svg
import Svg.Attributes as SA
import Svg.Events as SE exposing (on)

import Bootstrap.Button as Button

import Time exposing (every)

import Random exposing (Generator)

-- apparently xlinkHref is deprecated
import VirtualDom exposing (Attribute, attribute)
href : String -> Attribute msg
href value =
  VirtualDom.attribute "href" value

port resetSticks: () -> Cmd msg

main : Program Flags Model Msg
main =
  Browser.element
    { init = init
    , subscriptions = subscriptions
    , update = update
    , view = view
    }

type alias Flags = ()

type alias Model =
  { gs          : GameState
  , roll        : Maybe Int
  , selected    : Maybe Int
  , highlighted : List Int -- or BT.Tree
  , ts          : ThunkState
  , skippedMove : Bool
  , whitePlayer : PlayerType
  , blackPlayer : PlayerType
  , queuedAI    : Bool
  }

type PlayerType
  = Human
  | AIRand
  | AILast
  | AIFast
  | AIMed
  | AISlow

-- smol helper in case of later changes
isPlayerAI : PlayerType -> Bool
isPlayerAI player =
  case player of
    Human -> False
    _     -> True


initModel : Model
initModel =
  { gs          = initGame
  , roll        = Nothing
  , selected    = Nothing
  , highlighted = []
  , ts          = newNode initGame
  , skippedMove = False
  , blackPlayer = Human
  , whitePlayer = Human
  , queuedAI    = False
  }

type Msg
  = NewTurn     -- things to do at the beginning of a new turn
  | QueryRoll   -- request random number
  | GetRoll Int -- receive random number
  | Click Int   -- select piece
  | QueueAI     -- request an upcoming call of the AI
  | QueryAI     -- ask the AI to select and play piece
  | QueryRandMove    -- ask for a random move
  | PlayRandMove Int -- play the random move
  | Play
  | Skip
  | ChangePlayer Player PlayerType -- choose who's playing
  | Noop
  | Reset
    -- Somewhat redundant for new interface:
  | Unselect    -- unselect piece (if you click elsewhere)

------ INIT ------
init : Flags -> (Model, Cmd Msg)
init () =
  (initModel, Cmd.none)

------ SUBSCRIPTIONS ------
-- Not actually using any at the moment
subscriptions : Model -> Sub Msg
subscriptions model =
  let
    currPlayer =
      case model.gs.turn of
        White -> model.whitePlayer
        Black -> model.blackPlayer
  in
    if model.queuedAI then
      Time.every
        ( case currPlayer of
            Human  ->  10
            AIRand -> 150
            AILast -> 150
            AIFast -> 150
            AIMed  -> 100
            AISlow ->  50
        )
        (if currPlayer == AIRand
         then always QueryRandMove
         else always QueryAI)
    else
      Sub.none

------ UPDATE ------
update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    NewTurn ->
      if isOver model.gs /= NotDone then
        (model, Cmd.none)
      else
        case getPlayerType model of
          Human ->
            (model, Cmd.none)
          _ ->
            update QueryRoll model
    QueryRoll ->
      -- make sure that there are legal moves available!!
      -- if not, the turn should be skipped
      case model.roll of
        Nothing ->
          ( { model | skippedMove = False}
          , Random.generate GetRoll rollGenerator
          )
        _ ->
          -- no re-rolls!
          (model, Cmd.none)
    GetRoll i ->
      let
        newModel = model |> setRoll i |> highlightPieces
      in
        case getPlayerType newModel of
          Human ->
            -- (newModel, Cmd.none)
            (newModel, resetSticks ())
          _ ->
            update QueueAI newModel
    Click n ->
      let
        play m =
            (model
                |> unselectPiece
                |> tryPlay m
                |> Maybe.withDefault model
            , Cmd.none
            ) |> opChain (update NewTurn)
        unselect () =
          (unselectPiece model |> highlightPieces, Cmd.none)
        -- select the piece if it's the right color
        select () =
          case BT.getElem n model.gs.board of
            Just (Occ col) ->
              if col == currTurn then
                (selectPiece n model |> highlightPieces, Cmd.none)
              else
                unselect ()
            _ ->
              unselect ()
        currTurn = model.gs.turn

        checkSquare : Int -> (Model, Cmd Msg)
        checkSquare m =
          if n /= m
          then select ()
          else unselect ()
      in
        model.selected |> Maybe.map (\m ->
        model.roll     |> Maybe.map (\r ->
        if n == (m+r) || n >= 30 then
          -- n>=30 is for pieces that will leave the board
          -- Allow selecting a pawn of the same color
          -- that is being ``attacked'' (otherwise
          -- the click would be considered illegal)
          if Just (Occ model.gs.turn)
             /= BT.getElem n model.gs.board
          then play m
          else select () -- tries to select
        else
          checkSquare m
        ) |> Maybe.withDefault (checkSquare m) -- no roll
        ) |> Maybe.withDefault (select ()) -- no selection
    QueueAI ->
      ( { model | queuedAI = True }
      , Cmd.none)
    QueryAI ->
      -- Selects then plays a piece given by the AI
      let resetModel = { model | queuedAI = False } in
      model.roll |> Maybe.map (\roll ->
      let
        -- for now don't save results
        ts =
          let
            (N gs tmt) = model.ts
          in
            if gs == model.gs then
              model.ts
            else
              newNode model.gs

        -- Or for medium it could evaluate
        -- with ply 4 if any piece is on square 25
        ply =
          case getPlayerType model of
            AIFast -> 2
            AISlow -> 4
            AIMed  ->
              let
                isAPieceOn : Int -> Bool
                isAPieceOn sq =
                  BT.getElem sq model.gs.board |> Maybe.map (\s ->
                  case s of
                    Free  -> False
                    Occ _ -> True
                    -- Occ col -> model.gs.turn == col
                  ) |> Maybe.withDefault False
                occupancy =
                  [21, 22, 23, 24]
                    |> List.map isAPieceOn
                    |> List.foldl (||) False
              in
                if occupancy then 4 else 2
            _ -> 2

        (mp, newTS) =
          case getPlayerType model of
            AILast ->
              lastPawnAI (model.gs.turn) roll ts
            _ ->
              aiChooseMove (model.gs.turn) ply roll ts
      in
        if getPlayerType model == Human then
          -- don't want to make the play for the human
          case mp of
            Just p  -> resetModel |> update (Click p.square)
            Nothing -> resetModel |> update Noop
        else
          -- make the play!
          case mp of
            Just p ->
              resetModel |> setTS newTS
                         |> update (Click p.square)
                         |> opChain (update Play)
            Nothing ->
              resetModel |> setTS newTS
                         |> update Skip
                         |> opChain (update NewTurn)
      ) |> Maybe.withDefault (resetModel, Cmd.none)

    QueryRandMove ->
      -- send a signal to request a random move
      case model.roll of
        Nothing -> (model, Cmd.none)
        Just r  ->
          let
            numtot =
              findMoves model.gs
              |> BT.getElem r
              |> Maybe.map List.length
              |> Maybe.withDefault 0
            moveGen = Random.int 0 (numtot-1)
          in
            if numtot == 0 then
              { model | gs = skipTurn model.gs }
                |> clearRoll
                |> highlightPieces
                |> update NewTurn
            else
              (model, Random.generate PlayRandMove moveGen)
    PlayRandMove i ->
      -- choose the ith legal pawn
      model.roll |> Maybe.map (\r  ->
        let
          (mp, newTS) =
            BT.getElem (r-1) (findMoves model.gs) |> Maybe.andThen (\mvs ->
            List.drop i mvs |> List.head
            ) |> Maybe.withDefault (Nothing, newNode (skipTurn model.gs))
        in
        case mp of
          Just p ->
            model |> setTS newTS
                  |> update (Click p.square)
                  |> opChain (update Play)
          Nothing ->
            model |> setTS newTS
                  |> update Skip
                  |> opChain (update NewTurn)

      ) |> Maybe.withDefault (model, Cmd.none)

    Play ->
      let
        newModel =
          model.selected |> Maybe.andThen (\nn ->
            model
            |> unselectPiece
            |> tryPlay nn
          )
      in
        case newModel of
          Nothing ->
            (model, Cmd.none)
          Just nm ->
            update NewTurn nm
    Skip ->
      ( { model
        | gs = switchTurn model.gs
        } |> clearRoll
      , Cmd.none
      ) |> opChain (update NewTurn)
    ChangePlayer col ptype ->
      -- should this automatically send a query?
      ( setPlayerType col ptype model
      , Cmd.none)
        -- |> opChain
        -- (if isPlayerAI ptype
        -- then update QueueAI
        -- else update Noop)
    Noop ->
      (model, Cmd.none)
    Reset ->
      ( { initModel
        | blackPlayer = model.blackPlayer
        , whitePlayer = model.whitePlayer
        }
      , Cmd.none)
    Unselect ->
      (unselectPiece model |> highlightPieces, Cmd.none)


------ Helper functions for Update ------

-- Equivalent to throwing 4 coins (0/1)
-- and taking the sum. 0 becomes a roll
-- of 5 to make things more interesting
-- Note that 1-3 become by far the most
-- common rolls (7/8 of the time)
rollGenerator : Generator Int
rollGenerator =
  Random.weighted
    (4, 1)
    [ (6, 2)
    , (4, 3)
    , (1, 4)
    , (1, 5)
    ]

setRoll : Int -> Model -> Model
setRoll i model =
  { model | roll = Just i }

clearRoll : Model -> Model
clearRoll model =
  { model | roll = Nothing }

selectPiece : Int -> Model -> Model
selectPiece n model =
  { model | selected = Just n }

unselectPiece : Model -> Model
unselectPiece model =
  { model | selected = Nothing }

setTS : ThunkState -> Model -> Model
setTS ts model =
  { model | ts = ts }

setPlayerType : Player -> PlayerType -> Model -> Model
setPlayerType col ptype model =
  case col of
    White ->
      { model | whitePlayer = ptype}
    Black ->
      { model | blackPlayer = ptype}

getPlayerType : Model -> PlayerType
getPlayerType model =
  case model.gs.turn of
    White ->
      model.whitePlayer
    Black ->
      model.blackPlayer

-- calls Logic.makeMove and clears the model's roll
tryPlay : Int -> Model -> Maybe Model
tryPlay n model =
  model.roll            |> Maybe.andThen (\r ->
  getPawn  n model.gs   |> Maybe.andThen (\p ->
  makeMove p r model.gs |> Maybe.map (\js ->
  clearRoll { model | gs = js }
  )))

pawnSendBack : Model -> Model
pawnSendBack model =
  let
    dest = lastFreeBy 14 model.gs
    trySendFrom sq js =
      dest |> Maybe.andThen (\lf ->
      case BT.getElem sq js.board of
        Just (Occ col) ->
          if col == js.turn then
            pawnSwap sq lf js
          else
            Nothing
        _ -> Nothing
      ) |> Maybe.withDefault js
  in
    { model
    | gs = model.gs
      |> trySendFrom 27
      |> trySendFrom 28
      |> trySendFrom 29
    }


highlightPieces : Model -> Model
highlightPieces model =
  let
    currPlayer =
      case model.gs.turn of
        White -> model.whitePlayer
        Black -> model.blackPlayer
  in
    Maybe.withDefault { model | highlighted = [] } <|
    case model.selected of
      Nothing ->
        model.roll |> Maybe.map (\roll ->

        if isPlayerAI currPlayer then model else
        case List.map (\p -> p.square) (legalMoves model.gs roll) of
          [] ->
            -- if there are no moves, switch to the next turn
            -- but also send back pieces in the end zone
            -- and leave an indicator
            let
              newModel = pawnSendBack model
              sm = { newModel | skippedMove = True, highlighted = [] }
            in
              if newModel == model then
                sm
              else
                highlightPieces sm
          moves ->
            { model | highlighted = moves }
        )
      Just numpawn ->
        model.roll               |> Maybe.andThen (\roll ->
        getPawn numpawn model.gs |> Maybe.map (\spawn ->
        if isLegal model.gs.board spawn roll then
          { model
          | highlighted = [spawn.square, (spawn.square + roll)]
          }
        else
          { model
          | highlighted = [spawn.square]
          }
        ))


-- chain updates more easily
opChain : (Model -> (Model, Cmd Msg)) -> (Model, Cmd Msg) -> (Model, Cmd Msg)
opChain op (model, cmsg) =
  let
    (newModel, newMsg) = op model
  in
    (newModel, Cmd.batch [cmsg, newMsg])

------ VIEW ------

newline = br [] []
centering = HA.align "center"
monospace = HA.style "font-family" "monospace"

selectionColor = "palegreen"
-- "darkseagreen"
-- "#E65E5E"
-- "limegreen"
-- "lightgreen"

svgSquare : Int -> Model -> Int -> Int -> Int -> Html.Html Msg
svgSquare length model n i j =
  -- n for square number
  -- i for row, j for col
  let
    rlen = length // 10
    slen = length - rlen
    x = j*length + rlen//2
    y = i*length + rlen//2
    sqRect =
      [ Svg.rect
        [ SA.x      <| String.fromInt x
        , SA.y      <| String.fromInt y
        , SA.width  <| String.fromInt slen
        , SA.height <| String.fromInt slen
        -- make it rounded
        , SA.rx <| String.fromInt rlen
        , SA.ry <| String.fromInt rlen
        -- pick colors
        , SA.stroke <|
            if List.member n model.highlighted then selectionColor
            else "black"
        , SA.strokeWidth <|
            if List.member n model.highlighted then "5"
            else "1"
        , SA.fill <|
            if 0 == modBy 2 n
            then "beige"
            else "burlywood"
        , SE.onClick (Click n)
        ] []
      ]
    piece mp =
      case mp of
        Just p ->
          [ Svg.circle
            [ SA.cx <| String.fromInt (x+slen//2)
            , SA.cy <| String.fromInt (y+slen//2)
            , SA.r  <| String.fromInt (2*slen//7)
            , SA.stroke "black"
            , SA.fill <|
                if p.color == White
                then "snow"
                else "slategray"
            ,  SE.onClick (Click n)
            ] []
          ]
        Nothing ->
          []
    maybePawn = getPawn n model.gs
    picSize = "90"
    sqImage =
      let
        attrList =
          [ SA.x <| String.fromInt x
          , SA.y <| String.fromInt y
          , SA.width picSize, SA.height picSize
          , SE.onClick (Click n)
          ]
      in
        case squareType n of
          Rebirth ->
            [ Svg.image
              ((href "images/rebirth.svg") :: attrList) []
              -- ((href "images/rebirth.png") :: attrList) []
            ]
          Spec Happy ->
            [ Svg.image
              ((href "images/happy.svg") :: attrList) []
              -- ((href "images/happy.png") :: attrList) []
            ]
          Spec Water ->
            [ Svg.image
              ((href "images/water.svg") :: attrList) []
              -- ((href "images/water.png") :: attrList) []
            ]
          Spec Truths ->
            [ Svg.image
              ((href "images/three.svg") :: attrList) []
              -- ((href "images/three.png") :: attrList) []
            ]
          Spec Reatoum ->
            [ Svg.image
              ((href "images/two.svg") :: attrList) []
              -- ((href "images/two.png") :: attrList) []
            ]
          Spec Horus ->
            [ Svg.image
              ((href "images/horus.svg") :: attrList) []
              -- ((href "images/horus.png") :: attrList) []
            ]
          _ ->
            []
  in
    Svg.svg []
      (sqRect ++ sqImage ++ piece maybePawn)

-- make svg table
svgBoard : Model -> Html.Html Msg
svgBoard model =
  let
    length = 100
    ls = List.range 0 9
    line1 =
      ls |> List.map (\i ->
      svgSquare length model i 0 i)
    line2 =
      ls |> List.map (\i ->
      svgSquare length model (19-i) 1 i)
    line3 =
      ls |> List.map (\i ->
      svgSquare length model (20+i) 2 i)
    background =
      Svg.rect
        [ SA.x "-10"
        , SA.y "-10"
        , SA.rx "10"
        , SA.ry "10"
        , SA.width "1020"
        , SA.height "320"
        , SA.fill "gray"
        ] []
  in
    Svg.svg [ SA.viewBox "-50 -20 1060 350"]
      ( [background]
      ++ line1 ++ line2 ++ line3
      )

txtBoard : Model -> Html Msg
txtBoard model =
  let
    (l1, l2, l3) = boardToStrings model.gs.board
  in
    Html.div [centering, monospace]
        [ text "Mini ASCII Board"
        , newline
        , text" (P=White; Q=Black)"
        , newline
        , newline
        , text l1
        , newline
        , text l2
        , newline
        , text l3
        , newline
        ]

-- Table of HTML buttons
-- uses the basic single-character representation
buttonBoard : Model -> Html Msg
buttonBoard model =
  let
    makeButton i =
      button [ HE.onClick (Click i), monospace ]
        [ text
            <| String.fromList
            <| (\c -> [c])
            <| squareToChar i
            <| BT.getElem i model.gs.board
        ]
    line1 = List.map makeButton (List.range 0 9)
    line2 = List.map makeButton (List.reverse <| List.range 10 19)
    line3 = List.map makeButton (List.range 20 29)
  in
    Html.div [centering]
      (   line1 ++ [newline]
       ++ line2 ++ [newline]
       ++ line3 ++ [newline]
      )


scoreboard : Int -> Player -> Html.Html Msg
scoreboard n color =
  let
    rlen = (2*90//7)
    spacing = 2*rlen//3
    width = (initPawnCount-1+2) * spacing + 2*rlen + 4
    makepiece x y =
      [ Svg.circle
        [ SA.cx <| String.fromInt (x + 1)
        , SA.cy <| String.fromInt (y + 50)
        , SA.r  <| String.fromInt rlen
        , SA.stroke "black"
        , SA.fill <|
            if color == White
            then "snow"
            else "slategray"
        ] []
      ]
    centers =
      if 0 == modBy 2 n then
        List.map (\x -> width//2 + (x - n//2) * spacing) (List.range 1 n)
      else
        List.map (\x -> width//2 + (x - (n+1)//2) * spacing) (List.range 1 n)
    outline =
      Svg.rect
        [ SA.x "0"
        , SA.y "0"
        , SA.rx "10"
        , SA.ry "10"
        , SA.height "100"
        , SA.fill "antiquewhite"
        , SA.stroke "none"
        , SA.width <| String.fromInt width
        , HE.onClick (Click 30)
        ] []
  in
  Svg.svg
    [ SA.width "80%"
    , SA.viewBox ("0 0 " ++ String.fromInt width ++ " 100")
    ]
    (outline :: (List.concatMap (\x -> makepiece x 0) centers))

view : Model -> Html Msg
view model =
  let
    title =
      Html.h1 [ centering ]
        [ text "Senet" ]
    turn =
      Html.h2 [centering]
        [ text <|
            case isOver model.gs of
              NotDone ->
                if model.gs.turn == White
                then "White's turn"
                else "Black's turn"
              Won White ->
                "White won!"
              Won Black ->
                "Black won!"
        ]
    wscoreboard =
      let wpawn = (initPawnCount - model.gs.whitePawnCnt) in
      Html.h3 [centering]
        [ text <| "White: " ++ (String.fromInt wpawn) ]
    bscoreboard =
      let bpawn = (initPawnCount - model.gs.blackPawnCnt) in
      Html.h3 [centering]
        [ text <| "Black: " ++ (String.fromInt bpawn) ]
    selector col =
      Html.select
        [ HA.name <|
            case col of
              Black -> "Player 1 (Black)"
              White -> "Player 2 (White)"
        ]
        [ Html.option
          [HA.value "Human"
          , HE.onClick (ChangePlayer col Human)]
          [text "Human"]
        , Html.option
          [HA.value "AILast"
          , HE.onClick (ChangePlayer col AIRand)]
          [text "Random pawn"]
        , Html.option
          [HA.value "AILast"
          , HE.onClick (ChangePlayer col AILast)]
          [text "Last pawn"]
        , Html.option
          [HA.value "AIFast"
          , HE.onClick (ChangePlayer col AIFast)]
          [text "AI (fast)"]
        , Html.option
          [HA.value "AIMed"
          , HE.onClick (ChangePlayer col AIMed)]
          [text "AI (medium)"]
        , Html.option
          [HA.value "AISlow"
          , HE.onClick (ChangePlayer col AISlow)]
          [text "AI (slow)"]
        ]
    afterlifeRect =
      div []
        [ text <|
          if promotionImminent then
            "Promotion available!"
          else "No promotion available"
        , Svg.svg
          [ SA.x "0"
          , SA.y "0"
          , SA.rx "10"
          , SA.ry "10"
          , SA.width "100%"
          , SA.viewBox "-10 -10 220 295"
          , SE.onClick (Click 30)
          ]
          [ Svg.image
            [ SA.x "0", SA.y "0"
            , href "images/afterlife.jfif"
            , SE.onClick (Click 30)
            ] []
          , Svg.rect
            [ SA.x "0"
            , SA.y "0"
            , SA.rx "10"
            , SA.ry "10"
            , SA.width "190"
            , SA.height "265"
            , SA.fill "none"
            , SA.strokeWidth "5"
            , SA.stroke <|
                if   promotionImminent
                then selectionColor
                else "none"
            ] []
          ]
        ]
    promotionImminent =
      model.selected |> Maybe.andThen (\p ->
      model.roll     |> Maybe.map     (\roll ->
      promotablePawn p roll
      )) |> Maybe.withDefault False
    pawnView =
      div [centering]
        [ Html.table
          [ HA.style "width" "80%" ]
          [ Html.tr [centering]
            [ Html.td [HA.style "width" "35%"]
              [ bscoreboard
              , scoreboard (initPawnCount - model.gs.blackPawnCnt) Black
              ]
            ,  Html.td [HA.style "width" "35%"]
              [ wscoreboard
              , scoreboard (initPawnCount - model.gs.whitePawnCnt) White
              ]
            , Html.td [HA.style "width" "30%"]
              [afterlifeRect]
            ]
          ]
        ]
    header =
      div []
        [ Html.table
          [ HA.style "width" "100%" ]
          [ Html.tr []
            [ Html.td [HA.style "width" "30%"]
              [ Html.table [HA.style "width" "100%"]
                [ Html.td [HA.style "width" "10%", centering] []
                , Html.td [HA.style "width" "40%", centering]
                  [ bscoreboard
                  , scoreboard (initPawnCount - model.gs.blackPawnCnt) Black
                  ]
                , Html.td [HA.style "width" "40%", centering]
                  [ wscoreboard
                  , scoreboard (initPawnCount - model.gs.whitePawnCnt) White
                  ]
                ]
              ]
            , Html.td [HA.style "width" "10%"] []
            , Html.td [HA.style "width" "20%"]
              [ centerHeader ]
            , Html.td [HA.style "width" "100", centering]
              [ svgSticks
                (not <| isPlayerAI currPlayer)
                model.roll
                QueryRoll
                [ SA.width "200"]
              , Html.h3 [centering]
                [ text <|
                    case model.roll of
                      Nothing -> "Toss sticks!"
                      Just r  -> ("Tossed a " ++ String.fromInt(r))
                ]
              ]
            , Html.td [HA.style "width" "20%", centering]
              [ text   "Player 1 (Black): ", selector Black
              , newline
              , text "\tPlayer 2 (White): ", selector White
              , newline
              , button
                [ HE.onClick Reset
                , HA.disabled
                    (NotDone == isOver model.gs)
                ]
                [ text "New game" ]
              ]
            ]
          ]
        ]
    currPlayer =
      case model.gs.turn of
        White -> model.whitePlayer
        Black -> model.blackPlayer
    centerHeader =
      div []
        [ title
        , turn
        , div [centering]
          [ button
            [ HE.onClick (QueryRoll)
            , HA.disabled
              (NotDone /= isOver model.gs
              || model.roll /= Nothing)
            ]
            [ text <|
              case model.roll of
                Just r  -> "Toss: " ++ (String.fromInt r)
                Nothing -> "Toss!"
            ]
          , text "\t"
          , button
            [ if currPlayer == AIRand then
                HE.onClick (QueryRandMove)
              else
                HE.onClick (QueryAI)
            , HA.disabled
                  (NotDone /= isOver model.gs
                  || model.queuedAI)
            ]
            [ text <|
              if model.queuedAI
              then "Thinking..."
              else "Ask the AI!"
            ]
          , newline
          , button
            [ HE.onClick (Play)
            , HA.disabled
                (Nothing == model.selected
                || NotDone /= isOver model.gs)
            ]
            [ text <|
              case model.selected of
                Just s ->
                  if promotionImminent then
                    "Promote pawn on square " ++ (String.fromInt (s+1))
                  else
                    "Play pawn on square " ++ (String.fromInt (s+1))
                Nothing -> "Select a piece"
            ]
          , text "\t"
          , button
            [ HE.onClick Skip
            , HA.disabled
                (not model.skippedMove
                || NotDone /= isOver model.gs)
            ]
            [ text "Skip turn" ]
          ]
        ]
  in
    div
      []
      [ header
      , Html.table
        [ HA.style "width" "100%" ]
        [ Html.tr []
          [ Html.td [HA.style "width" "85%"]
            [svgBoard model]
          , Html.td [HA.style "width" "15%", centering]
            [afterlifeRect]
          ]
        ]
      , Html.table
        [ HA.style "width" "100%" ]
        [ Html.tr []
          [ Html.td [HA.style "width" "20%"] []
          , Html.td [HA.style "width" "60%"]
            [ about
            , rules Noop
            , notes
            ]
          , Html.td [HA.style "width" "20%"] []
          ]
        ]
      , credits
      ]
