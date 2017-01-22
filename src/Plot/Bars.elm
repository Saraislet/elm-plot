module Plot.Bars
    exposing
        ( Attribute
        , StyleAttribute
        , DataTransformers
        , Data
        , toBarData
        )

{-|
  Attributes to alter the view of the bars.

    myBarsSerie : Plot.Element (Interaction YourMsg)
    myBarsSerie =
        bars
            [ Bars.maxBarWidthPer 85
            , Bars.stackByY
            , Bars.label
                [ Label.formatFromList [ "A", "B", "C" ] ]
            ]
            [ [ Bars.fill "blue", Bars.opacity 0.5 ]
            , [ Bars.fill "red" ]
            ]
            (Bars.toBarData
                { yValues = .revenueByYear
                , xValue = Just .quarter
                }
                [ { quarter = 1, revenueByYear = [ 10000, 30000, 20000 ] }
                , { quarter = 2, revenueByYear = [ 20000, 10000, 40000 ] }
                , { quarter = 3, revenueByYear = [ 40000, 20000, 10000 ] }
                , { quarter = 4, revenueByYear = [ 40000, 50000, 20000 ] }
                ]
            )

# Definition
@docs Attribute, StyleAttribute


# Custom data
@docs Data, DataTransformers, toBarData

-}

import Plot.Attributes as Attributes exposing (..)
import Plot.Types as Types exposing (..)


{-| -}
type alias Attribute msg =
    Bars msg -> Bars msg


{-| -}
type alias StyleAttribute msg =
    BarsStyle msg -> BarsStyle msg


{-| The data format the bars element requires.
-}
type alias Data =
    Group



-- STYLES


{-| The functions necessary to transform your data into the format the plot requires.
 If you provide the `xValue` with `Nothing`, the bars xValues will just be the index
 of the bar in the list.
-}
type alias DataTransformers data =
    { yValues : data -> List Value
    , xValue : Maybe (data -> Value)
    }


{-| This function can be used to transform your own data format
 into something the plot can understand.

    Bars.toBarData
        { yValues = .revenueByYear
        , xValue = Just .quarter
        }
        [ { quarter = 1, revenueByYear = [ 10000, 30000, 20000 ] }
        , { quarter = 2, revenueByYear = [ 20000, 10000, 40000 ] }
        , { quarter = 3, revenueByYear = [ 40000, 20000, 10000 ] }
        , { quarter = 4, revenueByYear = [ 40000, 50000, 20000 ] }
        ]
-}
toBarData : DataTransformers data -> List data -> List Data
toBarData transform allData =
    List.indexedMap
        (\index data ->
            { xValue = getXValue transform index data
            , yValues = transform.yValues data
            }
        )
        allData


getXValue : DataTransformers data -> Int -> data -> Value
getXValue { xValue } index data =
    case xValue of
        Just getXValue ->
            getXValue data

        Nothing ->
            toFloat index + 1
