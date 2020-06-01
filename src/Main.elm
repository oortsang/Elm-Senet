-- Jeffrey Huang and Oliver Tsang, Spring 2020
-- Senet in Elm (Course Project) for CS 223, Functional Programming
-- Main.elm: makes the html page and handles interactions

module Main exposing (..)

-- From our project
import Board exposing (..)
import BoardTree as BT exposing (..)

-- For html-side
import Browser
import Browser.Events
import Html exposing (Html)
import Html.Attributes
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
  | Play Int    -- play piece
  | Noop



-- INIT
init : Flags -> (Model, Cmd Msg)
init () =
  (initModel, Cmd.none)

-- SUBSCRIPTIONS
subscriptions : Model -> Sub msg
subscriptions model =
  Sub.batch
    [] -- add buttons... and listen for buttons too

-- UPDATE
update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    QueryRoll ->
      (model, Random.generate GetRoll rollGenerator)
    GetRoll i ->
      (setRoll i model, Cmd.none)
    Select n ->
      -- also want to change highlighting?
      (selectPiece n model, Cmd.none)
    Unselect ->
      -- also want to change highlighting?
      (unselectPiece model, Cmd.none)
    Play n ->
      (tryPlay n model, Cmd.none)
      -- make sure to remove the old roll result !
      -- can request next roll if desired
    Noop ->
      (model, Cmd.none)

-- Update helper functions

-- Need to fix to not be a uniform distribution
rollGenerator : Generator Int
rollGenerator =
  Random.int 1 5

setRoll : Int -> Model -> Model
setRoll i model =
  { model | roll = Just i }

selectPiece : Int -> Model -> Model
selectPiece n model =
  { model | selected = Just n }

unselectPiece : Model -> Model
unselectPiece model =
  { model | selected = Nothing }

tryPlay : Int -> Model -> Model
tryPlay n model =
  -- something with makeMove
  -- but also updating the current model...
  Debug.todo "asdfdf"

-- VIEW
view : Model -> Html Msg
view model =
  let
    newline = Html.br [] []
    centering = Html.Attributes.align "center"
    monospace = Html.Attributes.style "font-family" "monospace"

    (l1, l2, l3) = boardToStrings model.gs.board
    txtBoard =
      Html.div [centering, monospace]
        [ Html.text "Temporary ASCII Board (P=White; Q=Black)"
        , newline
        , Html.text l1
        , newline
        , Html.text l2
        , newline
        , Html.text l3
        ]
  in
    Html.div
      []
      [ Html.h3
          [ centering ]
          [ Html.text "Senet! Functionality coming soon..." ]
      , txtBoard
      ]
