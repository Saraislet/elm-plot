module AreaChart exposing (chart, code)

import Svg
import Svg.Attributes
import Plot exposing (..)
import Colors


data1 : List ( Float, Float )
data1 =
    [ ( 0, 8 ), ( 1, 13 ), ( 2, 14 ), ( 3, 12 ), ( 4, 11 ), ( 5, 16 ), ( 6, 22 ), ( 7, 32 ) ]


chart : List ( Float, Float ) -> Svg.Svg a
chart data =
    plot
        [ size ( 600, 250 ) ]
        [ area [ areaStyle [ ( "stroke", Colors.blueStroke ), ( "fill", Colors.blueFill ) ] ] data
        , xAxis [ axisStyle [ ( "stroke", Colors.axisColor ) ] ]
        ]


code =
    """
    chart : Svg.Svg a
    chart =
        plot
            [ size ( 600, 250 ) ]
            [ area
                [ areaStyle
                    [ ( "stroke", Colors.blueStroke )
                    , ( "fill", Colors.blueFill )
                    ]
                ]
                data
            , xAxis [ axisStyle [ ( "stroke", Colors.axisColor ) ] ]
            ]
    """
