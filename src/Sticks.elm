-- Jeffrey Huang and Oliver Tsang, Spring/Summer 2020
-- Senet in Elm (Course Project) for CS 223, Functional Programming
-- Sticks.elm: help handle the stick animations (in svg)

module Sticks exposing (svgSticks)

-- import Main exposing (Msg)

import Html
import Html.Events
import Svg
import Svg.Attributes as SA
import Svg.Events as SE exposing (on)

type Side = Light | Dark

retLight : Bool -> Side
retLight b =
  case b of
    True  -> Light
    False -> Dark

getStr : Side -> String
getStr side =
  case side of
    Light -> "floralwhite"
    -- Light -> "papayawhip"
    Dark  -> "rosybrown"

oppCol : Side -> Side
oppCol side =
  case side of
    Light -> Dark
    Dark  -> Light

singleStickStatic : Side -> Int -> Html.Html msg
singleStickStatic side num =
  let
    rc = "3"
    xInit = 15+30*num
    xInitStr = Debug.toString xInit
    sideStr = getStr side
  in
    Svg.rect
      [ SA.fill sideStr
      , SA.y "15"
      , SA.x xInitStr
      , SA.width "15"
      , SA.height "70"
      ] []


-- return an animated flip from the old side to the new side
singleStick : Side -> Int -> Html.Html msg
singleStick side num =
  let
    rc = "3"
    dur = (Debug.toString (1+(toFloat num)/12)) ++ "s"
    -- dur = "1s"

    flipStr = (getStr (oppCol side)) ++ ";"
    sideStr = (getStr side) ++ ";"

    xInit = 15+30*num
    xMid  = xInit + 7

    xInitStr = Debug.toString xInit
    xMidStr = Debug.toString xMid

    xVals =
      xInitStr
      ++ ";" ++ xMidStr
      ++ ";" ++ xInitStr
      ++ ";" ++ xMidStr
      ++ ";" ++ xInitStr


    spline0 = "0 0 0.55 0;"
    spline1 = "0 0 0.55 1;"
    keySplines =
      spline0 ++ spline1 ++ spline0 ++ spline1

    keyTimes =
      "0; 0.24;0.25; 0.74;0.75; 1"
  in
  Svg.rect
    [ SA.fill <| getStr side
    -- , SA.stroke "black"
    -- , SA.strokeWidth "0.5"
    , SA.y "15"
    , SA.x xInitStr
    , SA.width "15"
    , SA.height "70"
    ]
    [ Svg.animate
      [ SA.attributeName "fill"
      , SA.dur dur
      , SA.repeatCount rc
      , SA.values <|
        sideStr ++ sideStr
        ++ flipStr ++ flipStr
        ++ sideStr ++ sideStr
      , SA.keyTimes keyTimes
      ] []
    , Svg.animate
      [ SA.attributeName "x"
      , SA.dur dur
      , SA.repeatCount rc
      , SA.values xVals
      , SA.calcMode "spline"
      , SA.keySplines keySplines
      ] []
    , Svg.animate
      [ SA.attributeName "width"
      , SA.dur dur
      , SA.repeatCount rc
      , SA.values "15; 1; 15; 1; 15"
      , SA.calcMode "spline"
      , SA.keySplines keySplines
      ] []
    ]


svgSticks : Maybe Int -> msg -> Html.Html msg
svgSticks roll op =
  let
    clickable =
      Svg.rect
      [ SA.x "0"
      , SA.y "0"
      , SA.rx "10"
      , SA.ry "10"
      , SA.height "100"
      , SA.width "135"
      , SA.fill  "transparent"
      , SA.stroke "none"
      , Html.Events.onClick op
      ] []
    background =
      Svg.rect
        [ SA.x "0"
        , SA.y "0"
        , SA.rx "10"
        , SA.ry "10"
        , SA.height "100"
        , SA.width "135"
        , SA.fill "antiquewhite"
        , SA.stroke "none"
        ] []
    contents =
      case roll of
        Just r ->
          let
            _ = Debug.log "r0" r0
            r0 = modBy 5 r
            side0 = retLight (r0 > 0)
            side1 = retLight (r0 > 1)
            side2 = retLight (r0 > 2)
            side3 = retLight (r0 > 3)
          in
            [ background
            , singleStick side0 0
            , singleStick side1 1
            , singleStick side2 2
            , singleStick side3 3
            , clickable
            ]
        Nothing ->
          [ background
          , singleStickStatic Light 0
          , singleStickStatic Light 1
          , singleStickStatic Light 2
          , singleStickStatic Light 3
          , clickable
          ]
  in
    Svg.svg
      [ SA.viewBox "0 0 135 100"]
      contents