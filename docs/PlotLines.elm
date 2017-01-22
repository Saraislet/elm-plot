module PlotLines exposing (plotExample)

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
    "Lines"


id : String
id =
    "PlotLines"


data1 : List ( Float, Float )
data1 =
    [ ( 0, 0 ), ( 1, 0 ) ]


data2 : List ( Float, Float )
data2 =
    [ ( 0, 2 ), ( 1, 2 ) ]


view : Svg.Svg a
view =
    plot
        [ size ( 400, 300 )
        , margin ( 30, 30, 30, 30 )
        , Attributes.id "lines"
        ]
        [ line
            [ stroke blueStroke
            , strokeWidth 2
            ]
            data1
        , line
            [ stroke pinkStroke
            , strokeWidth 2
            ]
            data2
        , xAxis
            [ lineStyle
                [ stroke axisColor ]
            , tick [ values (ValuesFromDelta 1) ]
            ]
        , yAxis
            [ lineStyle
                [ stroke axisColor ]
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
            , domainLowest (min 0)
            ]
            [ line
                [ Style.stroke blueStroke
                , Style.strokeWidth 2
                ]
                data1
            , line
                [ Style.stroke pinkStroke
                , Style.strokeWidth 2
                ]
                data2
            , xAxis
                [ lineStyle
                    [ Style.stroke axisColor ]
                , Axis.tickDelta 1
                ]
            ]
    """
