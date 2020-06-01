-- Jeffrey Huang and Oliver Tsang, Spring 2020
-- Senet in Elm (Course Project) for CS 223, Functional Programming
-- Game.elm: manages the game logic

module Game exposing (..)

import Board exposing (..)

-- maybe just handle this in the webpage file
type alias GameModel =
  { gs : GameState }
