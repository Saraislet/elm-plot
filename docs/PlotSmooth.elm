module PlotSmooth exposing (plotExample)

import Svg
import Plot exposing (..)
import Plot.Attributes as Attributes exposing (..)
import Common exposing (..)


plotExample : PlotExample msg
plotExample =
    { title = title
    , code = code
    , view = ViewStatic view
    , id = id
    }


title : String
title =
    "Interpolation"


id : String
id =
    "PlotSmooth"


data1 : List ( Float, Float )
data1 =
    [ ( 0, 10 ), ( 0.5, 20 ), ( 1, -5 ), ( 1.5, 4 ), ( 2, -7 ), ( 2.5, 5 ), ( 3, 20 ), ( 3.5, 7 ), ( 4, 28 ) ]


view : Svg.Svg a
view =
    plot
        [ size plotSize
        , margin ( 10, 20, 40, 20 )
        , domainHighest (\y -> y + 1)
        , domainLowest (\y -> y - 1)
        ]
        [ area
            [ stroke pinkStroke
            , fill pinkFill
            , strokeWidth 1
            , interpolation Bezier
            ]
            data1
        , xAxis
            [ lineStyle [ stroke axisColor ]
            , tick [ values (ValuesFromDelta 1) ]
            ]
        ]


code : String
code =
    """
    view : Svg.Svg a
    view =
        plot
            [ size plotSize
            , margin ( 10, 20, 40, 20 )
            , domainHighest (\\y -> y + 1)
            , domainLowest (\\y -> y - 1)
            ]
            [ area
                [ Area.stroke pinkStroke
                , Area.fill pinkFill
                , Area.strokeWidth 1
                , Area.smoothingBezier
                ]
                data1
            , xAxis
                [ lineStyle [ Line.stroke axisColor ]
                , Axis.tickDelta 1
                ]
            ]
    """
