module Internal.Grid exposing (..)

import Svg
import Plot.Attributes exposing (..)
import Internal.Draw as Draw exposing (..)
import Internal.Line as Line
import Plot.Types as Types exposing (..)


getValues : List Float -> ValuesOption -> List Float
getValues tickValues values =
    case values of
        ValuesAuto ->
            tickValues

        ValuesFromList customValues ->
            customValues

        ValuesFromDelta delta ->
            [ 0, 2, 3 ]


view : Plot -> Grid a -> Svg.Svg a
view meta ({ values, classes, orientation } as config) =
    Svg.g
        [ Draw.classAttributeOriented "grid" orientation classes ]
        (viewLines meta config)


viewLines : Plot -> Grid a -> List (Svg.Svg a)
viewLines plot { values, lineStyle } =
    List.map (viewLine lineStyle plot) <| getValues plot.scales.y.ticks values


viewLine : Line a -> Plot -> Value -> Svg.Svg a
viewLine config meta position =
    Line.view meta
        config
        [ ( meta.scales.x.bounds.lower, position )
        , ( meta.scales.x.bounds.upper, position )
        ]
