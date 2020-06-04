-- Jeffrey Huang and Oliver Tsang, Spring 2020
-- Senet in Elm (Course Project) for CS 223, Functional Programming
-- Main.elm: makes the html page and handles interactions

-- TODO:
--   0. Add a way to promote the pawns
--   1. Figure out highlighting/selection logic
--   2. Also fix the rng to be not uniform
--   3. Should there be a button to ask for the roll?
--   3.5. Skip moves that don't have legal moves
--   4. Add images for the board
--   5. Check for game termination!



module Main exposing (..)

-- From our project
import Board exposing (..)
import Logic exposing (..)
import BoardTree as BT exposing (..)

-- For html-side
import Browser
import Browser.Events

import Html exposing (Html, text, button, div, br, h3)
import Html.Attributes
import Html.Events

import Svg
import Svg.Attributes as SA
import Svg.Events as SE exposing (on)

import Random exposing (Generator)


-- apparently xlinkHref is deprecated
import VirtualDom exposing (Attribute, attribute)
href : String -> Attribute msg
href value =
  VirtualDom.attribute "href" value


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
  , highlighted : List Bool -- or BT.Tree
  -- maybe something else??
  }

initModel : Model
initModel =
  { gs = initGame
  , roll = Nothing
  , selected = Nothing
  , highlighted = []
  }

type Msg
  = QueryRoll   -- request random number
  | GetRoll Int -- receive random number
  | Click Int  -- select piece
  | Noop
    -- Deprecated for new interface:
  | Unselect -- unselect piece (if you click elsewhere)
  | Play     -- deprecated -- use Click instead



------ INIT ------
init : Flags -> (Model, Cmd Msg)
init () =
  (initModel, Cmd.none)

------ SUBSCRIPTIONS ------
-- Not actually using any at the moment
subscriptions : Model -> Sub msg
subscriptions model =
  Sub.none

------ UPDATE ------
-- To do:
--   - select/unselect:
--       - check for legality of the move?
--         (maybe do this in subscriptions...)
--       - if we check here rather than Play,
--         what's good error feedback?
--       - how to determine highlighting?
--   - play: how to give error feedback?
--       - may not be needed if we check at selection stage
update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    QueryRoll ->
      -- make sure that there are legal moves available!!
      -- if not, the turn should be skipped
      case model.roll of
        Nothing ->
          (model, Random.generate GetRoll rollGenerator)
        _ ->
          -- no re-rolls!
          (model, Cmd.none)
    GetRoll i ->
      (setRoll i model, Cmd.none)
    Click n ->
      let
        play m =
            (model
                |> unselectPiece
                |> tryPlay m
                |> Maybe.withDefault model
            , Cmd.none)
        unselect () =
          (unselectPiece model, Cmd.none)
        -- select the piece if it's the right color
        select () =
          case BT.getElem n model.gs.board of
            Just (Occ col) ->
              if col == currTurn then
                (selectPiece n model, Cmd.none)
              else
                -- nothing ()
                unselect ()
            _ ->
              -- nothing ()
              unselect ()
        nothing () =
          (model, Cmd.none)
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
    Noop ->
      (model, Cmd.none)
    Unselect ->
      (unselectPiece model, Cmd.none)
    Play ->
      let
        newModel =
          model.selected |> Maybe.andThen (\nn ->
            model
            |> unselectPiece
            |> tryPlay nn
          ) |> Maybe.withDefault model
      in
        (newModel, Cmd.none)

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

-- calls Board.makeMove and clears the model's roll
tryPlay : Int -> Model -> Maybe Model
tryPlay n model =
  model.roll            |> Maybe.andThen (\r ->
  getPawn  n model.gs   |> Maybe.andThen (\p ->
  makeMove p r model.gs |> Maybe.map (\js ->
  clearRoll { model | gs = js })))
  -- { model | gs = js, roll = Nothing })))


------ VIEW ------

newline = br [] []
centering = Html.Attributes.align "center"
monospace = Html.Attributes.style "font-family" "monospace"


svgSquare : Int -> GameState -> Int -> Int -> Int -> Html.Html Msg
svgSquare length gs n i j =
  -- n for square number
  -- i for row, j for col
  let
    rlen = length // 10
    slen = length - rlen
    x = j*length + rlen//2
    y = i*length + rlen//2
    sqRect =
      [ Svg.rect
        [ SA.x      <| Debug.toString x
        , SA.y      <| Debug.toString y
        , SA.width  <| Debug.toString slen
        , SA.height <| Debug.toString slen
        -- make it rounded
        , SA.rx <| Debug.toString rlen
        , SA.ry <| Debug.toString rlen
        -- pick colors
        , SA.stroke "black"
        , SA.fill <|
            if 0 == modBy 2 n
            then "beige"
            else "burlywood"
        , SE.onClick (Click n)
        ]
        []
      ]
    piece mp =
      case mp of
        Just p ->
          [ Svg.circle
            [ SA.cx <| Debug.toString (x+slen//2)
            , SA.cy <| Debug.toString (y+slen//2)
            , SA.r  <| Debug.toString (2*slen//7)
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
    maybePawn = getPawn n gs
    picSize = "88"
    sqImage =
      let
        attrList =
          [ SA.x <| Debug.toString x
            , SA.y <| Debug.toString y
            , SA.width picSize, SA.height picSize
            , SE.onClick (Click n)
          ]
      in
        case squareType n of
          Rebirth ->
            [ Svg.image
              ((href "images/rebirth.png") :: attrList) []
            ]
          Spec Happy ->
            [ Svg.image
              ((href "images/happy.png") :: attrList) []
            ]
          Spec Water ->
            [ Svg.image
              ((href "images/water.png") :: attrList) []
            ]
          Spec Truths ->
            [ Svg.image
              ((href "images/three.png") :: attrList) []
            ]
          Spec Reatoum ->
            [ Svg.image
              ((href "images/two.png") :: attrList) []
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
      svgSquare length model.gs i 0 i)
    line2 =
      ls |> List.map (\i ->
      svgSquare length model.gs (19-i) 1 i)
    line3 =
      ls |> List.map (\i ->
      svgSquare length model.gs (20+i) 2 i)
    background =
      Svg.rect [SA.x "-10", SA.y "-10", SA.width "1020", SA.height "320", SA.fill "gray"] []
    afterlife =
      svgSquare length model.gs 30 3 9
  in
    Svg.svg [ SA.viewBox "-50 -50 1100 500"]
      ( [background]
      ++ line1 ++ line2 ++ line3
      ++ [afterlife]
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
      button [ Html.Events.onClick (Click i), monospace ]
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


view : Model -> Html Msg
view model =
  let
    title =
      Html.h1 [ centering ]
        [ text "Senet!" ]
    turn =
      Html.h2 [centering]
        [ text <|
            if model.gs.turn == White
            then "White's turn"
            else "Black's turn"
        ]
  in
    div
      []
      [ title
      , turn
      , div [centering]
          [ button [ Html.Events.onClick (QueryRoll)]
              [ text <|
                  case model.roll of
                    Just r  -> "Roll: " ++ (Debug.toString r)
                    Nothing -> "Roll!"
              ]
          , text "\t"
          , button [ Html.Events.onClick (Play)]
              [ text <|
                  case model.selected of
                    Just s  -> "Play piece on square " ++ (Debug.toString s)
                    Nothing -> "Select a piece"
              ]
              -- "Play piece: " ++ Debug.toString model.selected]
          ]
      , div [centering] [svgBoard model]

      -- , div [centering]
      --     [ button [ Html.Events.onClick (Play)]
      --         [text <| "Play piece: " ++ Debug.toString model.selected]
      --     , newline
      --     ]
      -- , newline
      -- , buttonBoard model
      , newline
      , txtBoard model
      , newline
      , newline
      ]
