module Docs exposing (..)

import Html exposing (div, text, h1, img)
import Html.Attributes exposing (style, src)
import Svg
import Svg.Attributes
import Plot exposing (..)


myCustomXTick : Float -> Svg.Svg a
myCustomXTick tick =
    Svg.text'
        [ Svg.Attributes.transform "translate(0, 8)"
        , Svg.Attributes.style "text-anchor: middle;"
        ]
        [ Svg.tspan [] [ Svg.text "⚡️" ] ]


myCustomLabel : Float -> Svg.Svg a
myCustomLabel tick =
    Svg.text'
        [ Svg.Attributes.transform "translate(-10, 4)"
        , Svg.Attributes.style "stroke: purple; text-anchor: end;"
        ]
        [ Svg.tspan [] [ Svg.text ((toString (round tick)) ++ " ms") ] ]


areaData : List ( Float, Float )
areaData =
    [ ( -50, 34 ), ( -30, 432 ), ( -20, 35 ), ( 2, 546 ), ( 10, 345 ), ( 30, -42 ), ( 90, 67 ), ( 120, 50 ) ]


lineData : List ( Float, Float )
lineData =
    [ ( -50, 34 ), ( -30, 32 ), ( -20, 5 ), ( 2, -46 ), ( 10, -99 ), ( 30, -136 ), ( 90, -67 ), ( 120, 10 ) ]

firstExampleAreaData : List ( Float, Float )
firstExampleAreaData =
    [ ( 0, 12 ), ( 1, 15 ), ( 2, 16 ), ( 3, 14 ), ( 4, 11 ), ( 5, 13 ), ( 6, 20 ), ( 7, 24 ) ]

main =
    div [ style [ ( "width", "600px" )
                , ( "padding", "80px auto" )
                , ( "font-family", "sans-serif" )
                , ( "color", "#7F7F7F" )
                , ( "text-align", "center" ) ] ]
        [ img [ src "logo.png", style [ ( "width", "100px" ), ( "height", "100px" ) ] ] []
        , h1 [ style [ ( "font-weight", "200" ) ] ] [ text "Elm Plot" ]
        , plot
          [ dimensions ( 600, 250 ) ]
          [ area [ stroke "6A6A6A", fill "#ccdeff" ] firstExampleAreaData
          , xAxis
              [ axisLineStyle [ ( "stroke", "#7F7F7F" ) ]
              , tickList [ 0, 1, 2, 3, 4, 5, 6, 7 ]
              ]
          , yAxis
              [ amountOfTicks 6
              ]
          ]
        ]
