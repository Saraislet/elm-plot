module PlotComposed exposing (view, code, id)

import Svg
import Plot exposing (..)
import Plot.Attributes as Attributes exposing (..)
import Common exposing (..)


id : String
id =
    "ComposedPlot"


data1 : List ( Float, Float )
data1 =
    [ ( -10, 14 ), ( -9, 5 ), ( -8, -9 ), ( -7, -15 ), ( -6, -22 ), ( -5, -12 ), ( -4, -8 ), ( -3, -1 ), ( -2, 6 ), ( -1, 10 ), ( 0, 14 ), ( 1, 16 ), ( 2, 26 ), ( 3, 32 ), ( 4, 28 ), ( 5, 32 ), ( 6, 29 ), ( 7, 46 ), ( 8, 52 ), ( 9, 53 ), ( 10, 59 ) ]


dataScat : List ( Float, Float )
dataScat =
    [ ( -8, 50 ), ( -7, 45 ), ( -6.5, 70 ), ( -6, 90 ), ( -4, 81 ), ( -3, 106 ), ( -1, 115 ), ( 0, 140 ) ]


isOdd : Int -> Bool
isOdd n =
    rem n 2 > 0


filterLabels : AxisLabelInfo -> Bool
filterLabels { index } =
    not (isOdd index)


toTickStyle : AxisLabelInfo -> List (Attribute (TickStyle msg))
toTickStyle { index } =
    if isOdd index then
        [ length 7
        , stroke "#e4e3e3"
        ]
    else
        [ length 10
        , stroke "#b9b9b9"
        ]


labelStyle : List (Attribute (LabelStyle msg))
labelStyle =
    [ fontSize 12
    , displace ( 0, -2 )
    ]


view : State -> Svg.Svg (Interaction c)
view state =
    plotInteractive
        [ size ( 800, 400 )
        , padding ( 40, 40 )
        , margin ( 15, 20, 40, 15 )
        , Attributes.id "ComposedPlot"
        ]
        [ horizontalGrid
            [ lineStyle [ stroke "#f2f2f2" ] ]
        , verticalGrid
            [ lineStyle [ stroke "#f2f2f2" ] ]
        , area
            [ stroke skinStroke
            , fill skinFill
            , opacity 0.5
            , interpolation Bezier
            ]
            (List.map (\( x, y ) -> ( x, toFloat <| round (y * 2.1) )) data1)
        , area
            [ stroke blueStroke
            , fill blueFill
            , interpolation Bezier
            ]
            data1
        , line
            [ stroke pinkStroke
            , interpolation Bezier
            , strokeWidth 2
            ]
            (List.map (\( x, y ) -> ( x, toFloat <| round y * 3 )) data1)
        , scatter
            []
            dataScat
        , yAxis
            [ anchor (AnchorInner)
            , clearIntersections
            , position (PositionLowest)
            , lineStyle
                [ stroke "#b9b9b9" ]
            , tick [ values (ValuesFromDelta 50) ]
            , label
                [ viewStatic labelStyle
                , format (FormatFromFunc (\{ value } -> toString value ++ " °C"))
                ]
            ]
        , xAxis
            [ clearIntersections
            , lineStyle
                [ stroke "#b9b9b9" ]
            , tick
                [ values (ValuesFromDelta 2.5)
                , viewDynamic toTickStyle
                ]
            , label
                [ viewStatic
                    [ fontSize 12
                    , stroke "#b9b9b9"
                    ]
                , format (FormatFromFunc (\{ value } -> toString value ++ " x"))
                ]
            ]
        , xAxis
            [ position (PositionLowest)
            , lineStyle [ stroke "#b9b9b9" ]
            , tick
                [ viewDynamic toTickStyle ]
            , label
                [ viewStatic
                    [ fontSize 12
                    , stroke "#b9b9b9"
                    ]
                , format
                    (FormatFromFunc
                        (\{ value, index } ->
                            if isOdd index then
                                ""
                            else
                                toString value ++ " t"
                        )
                    )
                ]
            ]
        , hint
            [ lineStyle [ stroke "#b9b9b9" ] ]
            (getHoveredValue state)
        ]


code : String
code =
    """
    isOdd : Int -> Bool
    isOdd n =
        rem n 2 > 0


    filterLabels : Axis.LabelInfo -> Bool
    filterLabels { index } =
        not (isOdd index)


    toTickStyle : Axis.LabelInfo -> List (Attribute msg)
    toTickStyle { index } =
        if isOdd index then
            [ length 7
            , stroke "#e4e3e3"
            ]
        else
            [ length 10
            , stroke "#b9b9b9"
            ]


    labelStyle : List (Attribute msg)
    labelStyle =
        [ fontSize 12
        , displace ( 0, -2 )
        ]


    view : State -> Svg.Svg (Interaction c)
    view state =
        plotInteractive
            [ size ( 800, 400 )
            , padding ( 40, 40 )
            , margin ( 15, 20, 40, 15 )
            ]
            [ horizontalGrid
                [ Grid.lines [ stroke "#f2f2f2" ] ]
            , verticalGrid
                [ Grid.lines [ stroke "#f2f2f2" ] ]
            , area
                [ stroke skinStroke
                , fill skinFill
                , opacity 0.5
                ]
                data1
            , area
                [ stroke blueStroke
                , fill blueFill
                ]
                data1
            , line
                [ stroke pinkStroke
                , strokeWidth 2
                ]
                data2
            , scatter
                []
                data3
            , yAxis
                [ anchor (AnchorInner)
                , clearIntersections
                , position (PositionLowest)
                , lineStyle
                    [ stroke "#b9b9b9" ]
                , Axis.tickDelta 50
                , Axis.label
                    [ view labelStyle
                    , format (\\{ value } -> toString value ++ " °C")
                    ]
                ]
            , xAxis
                [ clearIntersections
                , lineStyle
                    [ stroke "#b9b9b9" ]
                , Axis.tickDelta 2.5
                , Axis.tick
                    [ viewDynamic toTickStyle ]
                , Axis.label
                    [ view
                        [ fontSize 12
                        , stroke "#b9b9b9"
                        ]
                    , format (\\{ value } -> toString value ++ " x")
                    ]
                ]
            , xAxis
                [ position (PositionLowest)
                , lineStyle [ stroke "#b9b9b9" ]
                , Axis.tick
                    [ viewDynamic toTickStyle ]
                , Axis.label
                    [ view
                        [ fontSize 12
                        , stroke "#b9b9b9"
                        ]
                    , format
                        (\\{ value, index } ->
                            if isOdd index then
                                ""
                            else
                                toString value ++ " t"
                        )
                    ]
                ]
            , hint
                [ Hint.lineStyle [ ( "background", "#b9b9b9" ) ] ]
                (getHoveredValue state)
            ]
    """
