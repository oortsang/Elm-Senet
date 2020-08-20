-- Jeffrey Huang and Oliver Tsang, Spring/Summer 2020
-- Senet in Elm (Course Project) for CS 223, Functional Programming
-- Sticks.elm: help handle the stick animations (in svg)

module Sticks exposing (svgSticks)

-- import Main exposing (Msg)

import Html
import Html.Events as HE
import Html.Attributes as HA
import Html.Keyed as Keyed
import Svg
import Svg.Attributes as SA
import Svg.Events as SE exposing (on)

import VirtualDom exposing (node, attribute, property)

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
    xInitStr = String.fromInt xInit
    sideStr = getStr side
  in
    Svg.rect
      [ SA.fill sideStr
      , SA.y "15"
      , SA.x xInitStr
      , SA.width "15"
      , SA.height "70"
      , SA.class <| "stick"
      ] []


-- return an animated flip from the old side to the new side
singleStick : Side -> Int -> Html.Html msg
singleStick side num =
  let
    rc = "2"
    dur = (String.fromFloat (0.75+(toFloat num)/8)) ++ "s"
    -- dur = "1s"

    flipStr = (getStr (oppCol side)) ++ ";"
    sideStr = (getStr side) ++ ";"

    xInit = 15+30*num
    xMid  = xInit + 7

    xInitStr = String.fromInt xInit
    xMidStr = String.fromInt xMid

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
    -- begCond = "sticks.click"
    -- begCond = "clickable.click"
  in
  Svg.rect
    [ SA.fill <| getStr side
    , SA.y "15"
    , SA.x xInitStr
    , SA.width "15"
    , SA.height "70"
    , SA.class "stick"
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
      -- , SA.begin begCond
      ] []
    , Svg.animate
      [ SA.attributeName "x"
      , SA.dur dur
      , SA.repeatCount rc
      , SA.values xVals
      , SA.calcMode "spline"
      , SA.keySplines keySplines
      -- , SA.begin begCond
      ] []
    , Svg.animate
      [ SA.attributeName "width"
      , SA.dur dur
      , SA.repeatCount rc
      , SA.values "15; 1; 15; 1; 15"
      , SA.calcMode "spline"
      , SA.keySplines keySplines
      -- , SA.begin begCond
      ] []
    ]


svgSticks : Bool -> Maybe Int -> msg -> Html.Html msg
svgSticks anim roll op =
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
      , SA.id "clickable"
      , HE.onClick op
      ] []
    backgroundPulsing =
      Svg.rect
        [ SA.x "0"
        , SA.y "0"
        , SA.rx "10"
        , SA.ry "10"
        , SA.height "100"
        , SA.width "135"
        , SA.fill "antiquewhite"
        , SA.stroke "palegreen"
        , SA.strokeWidth "0"
        ]
        [ Svg.animate
          [ SA.attributeName "stroke-width"
          , SA.dur "4s"
          , SA.repeatCount "indefinite"
          , SA.values "1.5; 4; 1.5"
          ] []

        ]
    backgroundStatic =
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
            r0 = modBy 5 r
            side0 = retLight (r0 > 0)
            side1 = retLight (r0 > 1)
            side2 = retLight (r0 > 2)
            side3 = retLight (r0 > 3)
          in
            if anim then
              [ backgroundStatic
              , singleStick side0 0
              , singleStick side1 1
              , singleStick side2 2
              , singleStick side3 3
              , clickable
              ]
            else
              [ backgroundStatic
              , singleStickStatic side0 0
              , singleStickStatic side1 1
              , singleStickStatic side2 2
              , singleStickStatic side3 3
              , clickable
              ]
        Nothing ->
          [ backgroundPulsing
          , singleStickStatic Light 0
          , singleStickStatic Light 1
          , singleStickStatic Light 2
          , singleStickStatic Light 3
          , clickable
          ]
  in
    Html.div
      []
      [ Svg.svg
        [ SA.viewBox "-5 -5 145 110"
        , SA.id "sticks"]
        contents
      -- , Html.node "script"
      --   []
      --   [ Html.text <|
      --   """function resetSticks() {
      --       document.getElementById(\"sticks\").setCurrentTime(0);
      --   }"""
      --   ]
      ]