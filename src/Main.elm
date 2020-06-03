-- Jeffrey Huang and Oliver Tsang, Spring 2020
-- Senet in Elm (Course Project) for CS 223, Functional Programming
-- Main.elm: makes the html page and handles interactions

-- TODO:
--   1. Add board/buttons to HTML
--   2. Figure out highlighting/selection logic
--   3. Also fix the rng to be not uniform
--   3.5. Should there be a button to ask for the roll?
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
view : Model -> Html Msg
view model =
  let
    newline = br [] []
    centering = Html.Attributes.align "center"
    monospace = Html.Attributes.style "font-family" "monospace"

    (l1, l2, l3) = boardToStrings model.gs.board
    txtBoard =
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
    makeButton i =
      button [ Html.Events.onClick (Select i) ]
        [ text <| Debug.toString i ]
    line1 = List.map makeButton (List.range 0 9)
    line2 = List.map makeButton (List.reverse <| List.range 10 19)
    line3 = List.map makeButton (List.range 20 29)
    tmpBoard =
      Html.div [centering]
        (   line1 ++ [newline]
         ++ line2 ++ [newline]
         ++ line3 ++ [newline]
        )
  in
    div
      []
      [ Html.h3
          [ centering ]
          [ text "Senet! Functionality coming soon..."
          , newline
          , text <|
              if model.gs.turn == White then
                "White's turn"
              else
                "Black's turn"
          ]
      , txtBoard
      , newline
      , tmpBoard
      , newline
      , div [centering]
          [ button [ Html.Events.onClick (QueryRoll)]
              [text <| "Roll: " ++ Debug.toString model.roll]
          , newline
          , newline
          , button [ Html.Events.onClick (Play)]
            [text <| "Play piece: " ++ Debug.toString model.selected]
          ]
      -- , div []
      --   [ text <| Debug.toString model ]
      ]
