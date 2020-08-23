-- Jeffrey Huang and Oliver Tsang, Spring 2020
-- Senet in Elm (Course Project) for CS 223, Functional Programming
-- Docs.elm: Store the documentation that has no reliance on the game state

module Docs exposing (rules, about, notes, treePic, credits, myTab)

import Sticks exposing (..)
import Board  exposing (..)

import Html exposing (..)
import Html.Attributes as HA
import Html.Events as HE

import Svg
import Svg.Attributes as SA

import VirtualDom exposing (Attribute, attribute)
href : String -> Attribute msg
href value =
  VirtualDom.attribute "href" value

newline = br [] []
centering = HA.align "center"
monospace = HA.style "font-family" "monospace"

myTab = text "\u{00A0}\u{00A0}\u{00A0}\u{00A0}\u{00A0}\u{00A0}"

cr = 35
crs = "35"

-- helper functions for vector arithmetic
floatify (px, py) =
  (toFloat px, toFloat py)
vAdd (px, py) (qx, qy) =
  (px+qx, py+qy)
sMul a (px, py) = (a*px, a*py)
convComb a p q =
  vAdd
    (sMul a     p)
    (sMul (1-a) q)
mag (px, py) = sqrt (px*px + py*py)
norm (px, py) =
  let
    mag_ = mag (px, py)
  in
    (-py/mag_, px/mag_)

arrow : (Int, Int) -> (Int, Int) -> List (Html msg)
arrow (x, y) (w, z) =
  let
    vec = floatify (x-w, y-z)
    len = mag vec
    setback = sMul (25.0/len) vec
    pt1 = vAdd (floatify (w, z)) setback
    nvec = sMul 10 (norm vec)
    pt2 = vAdd pt1 nvec
    pt3 = vAdd pt1 (sMul -1.0 nvec)
  in
    [ Svg.path
        [ SA.stroke "orange"
        , SA.strokeWidth "3"
        , SA.fill "orange"
        , SA.d <| svgPathParse <|
          [ "M"
          , String.fromInt x
          , String.fromInt y
          , "L"
          , String.fromInt w
          , String.fromInt z
          ]
        ] []
    , Svg.path
        [ SA.stroke "orange"
        , SA.strokeWidth "2"
        , SA.fill "orange"
        , SA.d <| svgPathParse <|
          [ "M"
          , String.fromInt w
          , String.fromInt z
          , "L"
          , String.fromFloat <| Tuple.first  pt2
          , String.fromFloat <| Tuple.second pt2
          , "L"
          , String.fromFloat <| Tuple.first  pt3
          , String.fromFloat <| Tuple.second pt3
          ]
        ] []
    ]

treeNode : (Int, Int) -> String -> String -> List (Html msg)
treeNode (x, y) color str =
  let
    xc = String.fromInt x
    yc = String.fromInt y
    circ =
      Svg.circle
        [ SA.fill color
        , SA.strokeWidth "1.5"
        , SA.stroke "black"
        , SA.r crs
        , SA.cx xc
        , SA.cy yc
        , SA.opacity "45%"
        ] []
    lab =
      if String.isEmpty str then
        []
      else
        [ Svg.text_
          [ SA.strokeWidth "0"
          , SA.x xc
          , SA.y <| String.fromInt (y+4)
          , SA.textAnchor "middle"
          , SA.fontFamily "sans-serif"
          ] [ Svg.text str ]
        ]
  in
    circ :: lab

-- draws a line from the higher node to the lower node
drawEdge : (Int, Int) -> (Int, Int) -> List (Html msg)
drawEdge (x, y) (w, z) =
  let
    -- points above/below the circles
    -- ordered so pt1 is above pt2 (unless y,z are very close)
    off = cr -- +1 (doesn't look so good)
    ((x1, y1), (x2, y2)) =
      if (y < z) then
        ((x, y+off), (w, z-off))
      else
        ((w, z+off), (x, y-off))
  in
    [ Svg.line
        [ SA.x1 <| String.fromInt x1
        , SA.y1 <| String.fromInt y1
        , SA.x2 <| String.fromInt x2
        , SA.y2 <| String.fromInt y2
        , SA.strokeWidth "1"
        , SA.stroke "black"
        ] []
    ]


dotsFrom : (Int, Int) -> (Int, Int) -> List (Html msg)
dotsFrom (x, y) (w, z) =
  let
    off = cr+5
    vec = floatify (w-x, z-y)
    u = sMul (1/(mag vec)) vec
    lp = vAdd (floatify (x, y)) <| sMul off    u
    rp = vAdd (floatify (w, z)) <| sMul (-off) u
    pt1 = convComb 0.3 lp rp
    pt2 = convComb 0.5 lp rp
    pt3 = convComb 0.7 lp rp
    circler (px, py) =
      Svg.circle
        [ SA.cx <| String.fromFloat px
        , SA.cy <| String.fromFloat py
        , SA.r "1.5"
        , SA.fill "black"
        , SA.stroke "none"
        ] []
  in
    [ circler pt1
    , circler pt2
    , circler pt3
    ]

rcaption : (Int, Int) -> String -> List (Html msg)
rcaption = caption False

lcaption : (Int, Int) -> String -> List (Html msg)
lcaption = caption True

caption : Bool -> (Int, Int) -> String -> List (Html msg)
caption isLeft (x, y) str =
  [ Svg.text_
    [ SA.strokeWidth "0"
    , SA.x <| String.fromInt x
    , SA.y <| String.fromInt (y+4)
    , SA.textAnchor <| if isLeft then "start" else "end"
    , SA.fontFamily "serif"
    , SA.fontSize "18pt"
    ] [ Svg.text str ]
  ]

treePic : Html msg
treePic =
  let
    width  = 1600
    height = 500
    outline =
      Svg.rect
        [ SA.fill "none"
        , SA.stroke "black"
        , SA.strokeWidth "1"
        , SA.x "0"
        , SA.y "0"
        , SA.width  <| String.fromInt (width-1)
        , SA.height <| String.fromInt (height-1)
        ] []
    cenx = width//2
    root = (cenx, 50)
    c1 = (cenx-220, 150)
    ce = (cenx+220, 150)
    row0 =
      [ treeNode root "slategray" "s"
      ]
    row1 =
      [ drawEdge root c1
      , drawEdge root ce
      , treeNode c1 "slategray" "Pawn 1"
      , dotsFrom c1 ce
      , treeNode ce "slategray" "Pawn n"
      ]
    d1 = (cenx-340, 250)
    d2 = (cenx-100, 250)
    dd = (cenx+100, 250)
    de = (cenx+340, 250)
    row2 =
      [ drawEdge c1 d1
      , drawEdge c1 d2
      , drawEdge ce dd
      , drawEdge ce de
      , dotsFrom d1 d2
      , dotsFrom d2 dd
      , dotsFrom dd de
      , treeNode d1 "mediumpurple" "1"
      , treeNode d2 "mediumpurple" "5"
      , treeNode dd "mediumpurple" "1"
      , treeNode de "mediumpurple" "5"
      ]
    e1 = (cenx-340-150, 350)
    e2 = (cenx-340-50,  350)
    e5 = (cenx-340+150, 350)
    ea = (cenx+340-150, 350)
    eb = (cenx+340-50,  350)
    ee = (cenx+340+150, 350)
    row3 =
      [ drawEdge d1 e1
      , drawEdge d1 e2
      , dotsFrom e2 e5
      , drawEdge d1 e5
      , treeNode e1 "snow" "Pawn 1"
      , treeNode e2 "snow" "Pawn 2"
      , treeNode e5 "snow" "Pawn m"
      , dotsFrom e5 ea
      , drawEdge de ea
      , drawEdge de eb
      , drawEdge de ee
      , dotsFrom eb ee
      , treeNode ea "snow" "Pawn 1"
      , treeNode eb "snow" "Pawn 2"
      , treeNode ee "snow" "Pawn m'"
      ]
    dropRow (x, y) = (x, y+100)
    row4 =
      [ drawEdge e1 <| dropRow e1
      , drawEdge e2 <| dropRow e2
      , drawEdge e5 <| dropRow e5
      , drawEdge ea <| dropRow ea
      , drawEdge eb <| dropRow eb
      , drawEdge ee <| dropRow ee
      , dotsFrom (dropRow e2) (dropRow e5)
      , dotsFrom (dropRow e5) (dropRow ea)
      , dotsFrom (dropRow eb) (dropRow ee)
      , treeNode (dropRow e1) "mediumturquoise" "val(\u{22C5})"
      , treeNode (dropRow e2) "mediumturquoise" "val(\u{22C5})"
      , treeNode (dropRow e5) "mediumturquoise" "val(\u{22C5})"
      , treeNode (dropRow ea) "mediumturquoise" "val(\u{22C5})"
      , treeNode (dropRow eb) "mediumturquoise" "val(\u{22C5})"
      , treeNode (dropRow ee) "mediumturquoise" "val(\u{22C5})"
      ]
    comments =
      [ rcaption (cenx-560, 50)  "Current State"
      , rcaption (cenx-560, 150) "My pawn choice"
      , rcaption (cenx-560, 250) "Stick toss"
      , rcaption (cenx-560, 350) "Their pawn choice"
      , rcaption (cenx-560, 450) "State evaluation"
      , lcaption (cenx+560, 450) "State evaluation"
      , lcaption (cenx+500, 300) "Minimum"
      , lcaption (cenx+400, 200) "Expectation"
      , lcaption (cenx+270, 100) "Maximum"
      , arrow (cenx+620, 430) (cenx+550, 320)
      , arrow (cenx+530, 280) (cenx+470, 220)
      , arrow (cenx+450, 180) (cenx+350, 120)
      , arrow (cenx+260, 90)  (cenx+40, 50)
      ]
    objs =
      comments ++ row0 ++ row1 ++ row2 ++ row3 ++ row4
  in
  Svg.svg
    [ SA.viewBox <| "0 0 "++ (String.fromInt width) ++ " " ++ (String.fromInt height)
    , SA.width "100%" ]
    (List.concat objs)


plantPic : String -> Html msg
plantPic imgName =
  Svg.svg [ SA.viewBox "0 0 50 50", SA.width "80"]
    [ Svg.rect
      [ SA.width  "50"
      , SA.height "50"
      , SA.fill "antiquewhite"
      , SA.x  "0"
      , SA.y  "0"
      , SA.rx "5"
      , SA.ry "5"
      ] []
    , Svg.image
      [href imgName, SA.width "50"] []
    ]

svgPathParse : List (String) -> String
svgPathParse =
  (List.intersperse " ") >> String.concat


boardDir : Html msg
boardDir =
  let
    length = 100
    slen   = 90
    rlen   = 10
    getCornerPos (i, j) =
      ( j*length + rlen//2
      , i*length + rlen//2)

    minisquare (i, j) =
      let
        (x, y) = getCornerPos (i, j)
      in
        Svg.rect
          [ SA.x      <| String.fromInt x
          , SA.y      <| String.fromInt y
          , SA.width  <| String.fromInt slen
          , SA.height <| String.fromInt slen
          , SA.rx     <| String.fromInt rlen
          , SA.ry     <| String.fromInt rlen
          , SA.stroke "gray"
          , SA.fill "none"
          ] []

    background =
      Svg.rect
        [ SA.x "-10"
        , SA.y "-10"
        , SA.rx "10"
        , SA.ry "10"
        , SA.width "1020"
        , SA.height "320"
        , SA.stroke "gray"
        , SA.fill   "none"
        ] []
    sqnum n =
      (modBy 3 n, n//3)
    squareNums =
      (List.range 0 29) |> List.map sqnum
    squares =
      squareNums |> List.map minisquare
    centerer (c, d) =
      (c + slen//2, d + slen//2)
    indArrow (i, j) (k, l) =
      let
        (x, y) =
          getCornerPos (i, j)
            |> centerer
        (w, z) =
          getCornerPos (k, l)
            |> centerer
      in
        arrow (x, y) (w, z)
  in
    Svg.svg [SA.viewBox "-50 -20 1060 350", SA.width "30%"]
      (  background
      :: squares
      ++ indArrow (0, 0) (0, 5)
      ++ indArrow (0, 4) (0, 9)
      ++ indArrow (0, 9) (1, 9)
      ++ indArrow (1, 9) (1, 4)
      ++ indArrow (1, 5) (1, 0)
      ++ indArrow (1, 0) (2, 0)
      ++ indArrow (2, 0) (2, 5)
      ++ indArrow (2, 4) (2, 9)
      )



rules : msg -> Html msg
rules op =
  p []
    [ h2 [] [text "Rules of Senet"]
    , text """
      The set of rules used by Ancient Egyptians have been long lost.
      However, some historians have proposed rules that seem reasonable.
      Here, we implement rules based on
      """
    , a
      [ href "https://www.startwithabook.org/content/pdfs/EgyptianSenetGame.pdf"
      , HA.attribute "target" "_blank" -- open in new tab by default
      ]
      [text "Kendall's rules"]
    , text """.
      The objective of Senet is to promote all your pieces
      from the board. Pawns move across the board in a snake
      pattern:
      """
    , newline
    , newline
    , div [centering]
      [ boardDir ]
      -- [ text "<illustration>" ]
    , newline
    , text """
      At each move, you begin by throwing four sticks to determine
      how many squares you can move your pawns.
      """
    , h4 [] [text "Throwing sticks"]
    , text """
      There are four sticks, each with one light side and one dark side.
      Each dark side corresponds to a 1, and each light side
      corresponds to a 0. After throwing the four sticks,
      we add up the resulting 0/1 values. This is the number of
      squares you can move your pawns.
      However, if all sides come up dark, i.e. the total is 0,
      the pawns can be moved by 5 squares.
      """
    , newline
    , newline
    , table [ centering ]
      [ tr []
        [ td [HA.style "width" "120", centering]
          [svgSticks False (Just 2) op []]
        , td []
          [ text """
          Throw the sticks by pressing the picture of the sticks in the
          upper right-hand corner, or press the ''roll'' button.
          """
          ]
        ]
      ]
    , newline
    , h4 [] [text "Moving your pawn"]
    , text """
      In general, you can choose to move any pawn by the result of
      the stick toss (between 1 and 5). If your pawn lands on an
      opponent's pawn, then the two pawns swap positions.
      However, there are special rules restricting movement in the
      last five squares. (This is done to make pawn promotion more
      interesting)
      """
    , newline
    , newline
    , table [ centering ]
      [ tr []
        [ td [HA.style "width" "20%", centering]
          [plantPic "images/rebirth.svg"]
        , td []
          [ b [] [text "House of Rebirth: "]
          , text """This is where pawns are sent for
          ''rebirth.'' If this square is occupied, the pawn
          is sent to the first unoccupied square before this square.
          """]
        ]
      , tr []
        [ td [HA.style "width" "20%", centering]
          [plantPic "images/happy.svg"]
        , td []
          [ b [] [text "House of Happiness:"]
          , text """
          All pawns must pass through
          this square. Pawns are not allowed to jump over this
          square, and they must land precisely on this square.
          """]
        ]
      , tr []
        [ td [HA.style "width" "20%", centering]
          [plantPic "images/water.svg"]
        , td []
          [ b [] [text "House of Water:"]
          , text """
          If a pawn lands on this square, it must return to
          the House of Rebirth.
          """]
        ]
      , tr []
        [ td [HA.style "width" "20%", centering]
          [plantPic "images/three.svg"]
        , td []
          [ b [] [text "House of Three Truths:"]
          , text """
          If a pawn lands on this square, the following turn
          you can promote that pawn if you throw a 3 and play it.
          Otherwise, the pawn will be sent back to the House of Rebirth.
          """]
        ]
      , tr []
        [ td [HA.style "width" "20%", centering]
          [plantPic "images/two.svg"]
        , td []
          [ b [] [text "House of Re-Atoum:"]
          , text """
          If a pawn lands on this square, the following turn
          you can promote that pawn if you throw a 2 and play it.
          Otherwise, the pawn will be sent back to the House of Rebirth.
          """]
        ]
      , tr []
        [ td [HA.style "width" "20%", centering]
          [plantPic "images/horus.svg"]
        , td []
          [ b [] [text "House of Horus:"]
          , text """
          If a pawn lands on this square, the following turn
          you can promote it with any stick toss.
          However, if you do not play that pawn, it will be
          sent back to the House of Rebirth."""]
        ]
      ]
    , newline
    , text """
      Occasionally there may be no legal moves available. In this case,
      just skip your turn and sit tight until it's your turn again (and
      hope for better luck)!"""
    ]

notes : Html msg
notes =
  div []
    [ h4 [] [ text "Strategic tips" ]
    , text """
      1. Try to keep a pawn on the House of Happiness as long as you can!
      If no pawns are nearby, wait until you toss a 4 or 5 to move this
      pawn for a guaranteed promotion."""
    , newline, newline
    , text """
      2. Each of the sticks has a 50/50 chance of landing light or dark.
      The four collectively follow a """
    , a
      [ href "https://www.mathsisfun.com/data/binomial-distribution.html"
      , HA.attribute "target" "_blank" ]
      [ text "binomial distribution" ]
    , text """,
      which means that not all stick tosses are equally likely.
      Here's a table of the probabilities:
      """
    , newline, newline
    , table [centering, HA.attribute "border" "1"]
      [ tr []
        [ th [centering] [text "Stick toss"]
        , th [centering ] [text "Probability"]
        ]
      , tr []
        [ td [centering] [text "5"]
        , td [centering] (frac 1 16)
        ]
      , tr []
        [ td [centering] [text "1"]
        , td [centering] (frac 4 16)
        ]
      , tr []
        [ td [centering] [text "2"]
        , td [centering] (frac 6 16)
        ]
      , tr []
        [ td [centering] [text "3"]
        , td [centering] (frac 4 16)
        ]
      , tr []
        [ td [centering] [text "4"]
        , td [centering] (frac 1 16)
        ]
      ]
    , newline
    , text """
      Throwing a 2 is the single most likely option. This means
      that the House of Three Truths is something like 50% better than
      the House of Re-Atoum! You can also take advantage of this
      probability distribution to place your pawns strategically
      to increase your chances of landing on the House of Happiness
      (and avoid landing just 1 square before it).
      """
    , h2 [] [ text "Additional notes on gameplay" ]
    , h4 [] [ text "Computer players" ]
    , text """
      There are several computer players available which can be selected
      in the upper right-hand corner:"""
    , ul []
      [ b [] [ text "Random pawn:" ]
      , text " the computer chooses its move at random."
      , newline, newline
      , b [] [ text "Last pawn: " ]
      , text " the computer always plays the pawn closest to the finish line."
      , newline, newline
      , b [] [ text "AI (fast): " ]
      , text """
        the computer searches for the best move to increase its
        lead over the opponent. Looks 1 turns ahead."""
      , newline, newline
      , b [] [ text "AI (medium): " ]
      , text """
        the computer searches for the best move to increase its
        lead over the opponent. Looks 1 turn ahead unless a piece
        is close to the House of Happiness, in which case it looks
        ahead 2 turns."""
      , newline, newline
      , b [] [ text "AI (slow): " ]
      , text """
        the computer searches for the best move to increase its
        lead over the opponent. Looks 2 turns ahead."""
      ]
    , text """
      The AI (other than random and last pawn) constructs a move tree
      and  uses an approach known as
      """
    , a
      [ href "https://en.wikipedia.org/wiki/Expectiminimax"
      , HA.attribute "target" "_blank"
      ] [ text "expectiminimax" ]
    , text """
      to analyze these
      moves. It has a heuristic value function to indicate how good
      a given game state is for it. In this case, the value function
      looks at the number of squares the opponent needs to move their
      pawns end of the board, and the value function compares that to
      the number of squares the current player or AI needs to move its
      pawns to the end of the board."""
    , newline
    , myTab
    , text """
      The expectiminimax AI simulates all possible game states for the next
      turn or two. The computer evaluates each terminal game state
      (i.e., states beyond which the computer has not explored) using
      the heuristic value function."""
    , newline
    , myTab
    , text """
      The computer then goes back and assigns a value to the rest of
      the non-terminal states. For simplicity assume the computer is
      playing as black, and its opponent is white. Here's how it
      computes the value of a state:"""
    , ol []
      [ text """
        If it is black's turn, take the value of the state resulting
        from the strongest move black can make.
        This is the move with the maximum value."""
      , newline, newline
      , text """
        If it is white's turn, take the value of the state resulting
        from the strongest move white can
        make against black. This is the move with the minimum value."""
      ]
    , text """
      Since the stick toss introduces randomness, the value of a state
      is taken as a weighted average (expected value) of the minimum
      or maximum values of the next move. Here's a picture to help
      visualize the process:"""
    , newline
    , newline
    ]
frac : Int -> Int -> List (Html msg)
frac n d =
  [ node "sup" [] [text <| String.fromInt n]
  , text "\u{002F}"
  , node "sub" [] [text <| String.fromInt d]
  ]

about : Html msg
about =
  div []
    [ h2 [] [ text "A bit about Senet"]
    , text """
      In Ancient Egyptian, ''Senet'' meant \"passing,\" as in passing
      on to the afterlife. Indeed, many of our records of Senet were
      found in tombs. Some of the oldest boards date back to
      the Middle Kingdom (~ 2050 - 1710 BC).
      The game was first referenced in tomb paintings around
      the 25"""
    , node "sup" [] [text "th"]
    , text """ century BC. The game continued to be played for a long time,
      until it faded from use around the time Egypt came under Roman control
      (30 BC).
      For a game that existed for such a long time, it is not so surprising that
      we have found so many variants of boards. Check some out
      """
    , a
      [ href "https://journals.sagepub.com/doi/full/10.1177/0307513319896288"
      , attribute "target" "_blank"]
      [ text "here"]
    , text " or "
    , a
      [ href "https://egyptianmuseum.pastperfectonline.com/webobject/9AB20ABF-F246-475B-96C8-422545029070"
      , attribute "target" "_blank"]
      [ text "here"]
    , text "!"
    ]


credits : Html msg
credits =
  div []
    [ newline, newline, newline
    , text """Developed by Oliver Tsang and Jeffrey Huang
      in """
    , a [ href "https://elm-lang.org/"
        , attribute "target" "_blank"
        ] [text "Elm"]
    , text """ for UChicago's CMSC 22300 Functional Programming class.
      Special thanks to the """
    , a [ href "https://oi.uchicago.edu/"
        , attribute "target" "_blank"
        ] [text "Oriental Institute"]
    , text " for their support!"
    ]