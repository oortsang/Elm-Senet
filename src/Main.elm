-- Jeffrey Huang and Oliver Tsang, Spring 2020
-- Senet in Elm (Course Project) for CS 223, Functional Programming
-- Main.elm: makes the html page and handles interactions

-- TODO:
--   1. Add board/buttons to HTML
--   2. Figure out highlighting/selection logic
--   3. Also fix the rng to be not uniform
--   3.5. Should there be a button to ask for the roll?
--   3.6. Skip moves that don't have legal moves
--   4. Add images for the board



module Main exposing (..)

-- From our project
import Board exposing (..)
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
  | Select Int  -- select piece
  | Unselect    -- unselect piece (if you click elsewhere)
  | Play        -- play the selected piece
  | Noop



------ INIT ------
init : Flags -> (Model, Cmd Msg)
init () =
  (initModel, Cmd.none)

------ SUBSCRIPTIONS ------
subscriptions : Model -> Sub msg
subscriptions model =
  Sub.batch
    [] -- add buttons... and listen for buttons too

------ UPDATE ------
-- To do:
--   - check if GetRoll is working
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
    Select n ->
      -- also want to change highlighting?
      -- Should we check for legality?
      (selectPiece n model, Cmd.none)
    Unselect ->
      -- also want to change highlighting?
      (unselectPiece model, Cmd.none)
    Play ->
      -- TODO: Add error messages...
      let
        newModel =
          model.selected |> Maybe.andThen (\n ->
          model
            |> unselectPiece
            |> tryPlay n
          ) |> Maybe.withDefault model
      in
        (newModel, Cmd.none)
    Noop ->
      (model, Cmd.none)

------ Helper functions for Update ------

-- Need to fix to not be a uniform distribution
rollGenerator : Generator Int
rollGenerator =
  Random.int 1 5

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



-- make svg table
svgBoard : Model -> Html.Html msg
svgBoard model =
  Svg.svg
    [ SA.width  "1000"
    , SA.height "300"
    , SA.viewBox "0 0 300 1000"
    ]
    [ Svg.circle
        [ SA.cx "700"
        , SA.cy "50"
        , SA.r  "5"
        , SA.fill "green"
        ]
        []
    ]
  -- Debug.todo "Make svg board"

txtBoard : Model -> Html Msg
txtBoard model =
  let
    (l1, l2, l3) = boardToStrings model.gs.board
  in
    Html.div [centering, monospace]
        [ text "Temporary ASCII Board (P=White; Q=Black)"
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
      button [ Html.Events.onClick (Select i), monospace ]
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
      Html.h3 [ centering ]
        [ text "Senet!"
        , newline
        ]
    turn = 
      Html.div [centering]
        [ text <|
            if model.gs.turn == White then
              "White's turn"
            else
              "Black's turn"
        ]
  in
    div
      []
      [ title
      , turn
      , newline
      , txtBoard model
      , newline
      , div [centering]
          [ button [ Html.Events.onClick (QueryRoll)]
              [text <| "Roll: " ++ Debug.toString model.roll]
          , newline
          , newline
          ]
      , buttonBoard model
      , newline
      , div [centering]
          [ button [ Html.Events.onClick (Play)]
              [text <| "Play piece: " ++ Debug.toString model.selected]
          , newline
          ]
      , newline
      -- , div [centering] [svgBoard model]
      , newline
      ]
