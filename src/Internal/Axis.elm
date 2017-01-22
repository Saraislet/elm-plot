module Internal.Axis
    exposing
        ( view
        , getAxisPosition
        , getValues
        , getDelta
        )

import Internal.Tick as Tick
import Internal.Label as Label
import Internal.Line as Line
import Internal.Draw as Draw exposing (..)
import Internal.Scale exposing (..)
import Plot.Types as Types exposing (..)
import Plot.Attributes as Attributes
    exposing
        ( Plot
        , Orientation(..)
        , Axis
        , AxisLabelInfo
        , AnchorOption(..)
        , PositionOption(..)
        , ValuesOption(..)
        )
import Svg
import Svg.Attributes
import Round
import Regex


view : Plot -> Axis msg -> Svg.Svg msg
view plot ({ lineStyle, tick, label, orientation, clearIntersections, position, anchor, classes } as config) =
    let
        tickValues =
            toTickValues plot config

        labelValues =
            toLabelValues config tickValues

        axisPosition =
            getAxisPosition plot.scales.y position
    in
        Svg.g
            [ Draw.classAttributeOriented "axis" orientation classes ]
            [ viewAxisLine lineStyle plot axisPosition
            , Svg.g
                [ Svg.Attributes.class "elm-plot__axis__ticks" ]
                (List.map
                    (placeTick plot config axisPosition (Tick.toView tick))
                    (toIndexInfo orientation tickValues)
                )
            , Svg.g
                [ Svg.Attributes.class "elm-plot__axis__labels"
                , Svg.Attributes.style <| toAnchorStyle anchor orientation
                ]
                (Label.view label
                    (viewLabel plot config axisPosition)
                    (toIndexInfo orientation labelValues)
                )
            ]



-- View line


viewAxisLine : Attributes.Line msg -> Plot -> Float -> Svg.Svg msg
viewAxisLine config plot position =
    Line.view plot
        config
        [ ( plot.scales.x.bounds.lower, position )
        , ( plot.scales.x.bounds.upper, position )
        ]



-- View labels


viewLabel : Plot -> Axis msg -> Float -> AxisLabelInfo -> Svg.Svg msg -> Svg.Svg msg
viewLabel plot config axisPosition info view =
    Svg.g
        [ Svg.Attributes.transform (getLabelPosition plot config axisPosition info)
        , Svg.Attributes.class "elm-plot__axis__label"
        ]
        [ view ]


getLabelPosition : Plot -> Axis msg -> Float -> AxisLabelInfo -> String
getLabelPosition plot { orientation, anchor } axisPosition info =
    toSvgCoords plot.scales ( info.value, axisPosition )
        |> addDisplacement (getDisplacement anchor orientation)
        |> toTranslate



-- View ticks


placeTick : Plot -> Axis msg -> Float -> (AxisLabelInfo -> Svg.Svg msg) -> AxisLabelInfo -> Svg.Svg msg
placeTick plot ({ orientation, anchor } as config) axisPosition view info =
    Svg.g
        [ Svg.Attributes.transform <| (toTranslate <| toSvgCoords plot.scales ( info.value, axisPosition )) ++ " " ++ (toRotate anchor orientation)
        , Svg.Attributes.class "elm-plot__axis__tick"
        ]
        [ view info ]


getAxisPosition : Scale -> PositionOption -> Float
getAxisPosition { bounds } position =
    case position of
        PositionAtZero ->
            clamp bounds.lower bounds.upper 0

        PositionLowest ->
            bounds.lower

        PositionHighest ->
            bounds.upper


toAnchorStyle : Attributes.AnchorOption -> Orientation -> String
toAnchorStyle anchor orientation =
    case orientation of
        X ->
            "text-anchor: middle;"

        Y ->
            "text-anchor:" ++ getYAnchorStyle anchor ++ ";"


getYAnchorStyle : Attributes.AnchorOption -> String
getYAnchorStyle anchor =
    case anchor of
        Attributes.AnchorInner ->
            "start"

        Attributes.AnchorOuter ->
            "end"


{-| The displacements are just magic numbers, so no science. (Just defaults)
-}
getDisplacement : Attributes.AnchorOption -> Orientation -> Point
getDisplacement anchor orientation =
    case orientation of
        X ->
            case anchor of
                Attributes.AnchorInner ->
                    ( 0, -15 )

                Attributes.AnchorOuter ->
                    ( 0, 25 )

        Y ->
            case anchor of
                Attributes.AnchorInner ->
                    ( 10, 5 )

                Attributes.AnchorOuter ->
                    ( -10, 5 )


toRotate : Attributes.AnchorOption -> Orientation -> String
toRotate anchor orientation =
    case orientation of
        X ->
            case anchor of
                Attributes.AnchorInner ->
                    "rotate(180 0 0)"

                Attributes.AnchorOuter ->
                    "rotate(0 0 0)"

        Y ->
            case anchor of
                Attributes.AnchorInner ->
                    "rotate(-90 0 0)"

                Attributes.AnchorOuter ->
                    "rotate(90 0 0)"


filterValues : Plot -> Axis msg -> List Float -> List Float
filterValues plot config values =
    if config.clearIntersections then
        List.filter (isCrossing plot) values
    else
        values


isCrossing : Plot -> Float -> Bool
isCrossing plot value =
    not <| List.member value plot.scales.y.ticks



-- Remember we always assume we're working with the x-axis. scale.x is therefore
-- just the scale we work with. It will also be the right one for the y-axis.


toTickValues : Plot -> Axis msg -> List Value
toTickValues plot config =
    getValues config.tick.values plot.scales.x
        |> filterValues plot config


toLabelValues : Axis msg -> List Value -> List Value
toLabelValues config tickValues =
    case config.label.values of
        ValuesFromList values ->
            values

        ValuesFromDelta delta ->
            []

        ValuesAuto ->
            tickValues



-- Resolve values


getValues : ValuesOption -> Scale -> List Value
getValues config =
    case config of
        ValuesAuto ->
            toValuesAuto

        ValuesFromDelta delta ->
            toValuesFromDelta delta

        ValuesFromList values ->
            always values


getFirstValue : Float -> Float -> Float
getFirstValue delta lowest =
    ceilToNearest delta lowest


getCount : Float -> Float -> Float -> Float -> Int
getCount delta lowest range firstValue =
    floor ((range - (abs lowest - abs firstValue)) / delta)


getDeltaPrecision : Float -> Int
getDeltaPrecision delta =
    delta
        |> toString
        |> Regex.find (Regex.AtMost 1) (Regex.regex "\\.[0-9]*")
        |> List.map .match
        |> List.head
        |> Maybe.withDefault ""
        |> String.length
        |> (-) 1
        |> min 0
        |> abs


getDelta : Float -> Int -> Float
getDelta range totalTicks =
    let
        -- calculate an initial guess at step size
        delta0 =
            range / (toFloat totalTicks)

        -- get the magnitude of the step size
        mag =
            floor (logBase 10 delta0)

        magPow =
            toFloat (10 ^ mag)

        -- calculate most significant digit of the new step size
        magMsd =
            round (delta0 / magPow)

        -- promote the MSD to either 1, 2, or 5
        magMsdFinal =
            if magMsd > 5 then
                10
            else if magMsd > 2 then
                5
            else if magMsd > 1 then
                1
            else
                magMsd
    in
        (toFloat magMsdFinal) * magPow


toValue : Float -> Float -> Int -> Float
toValue delta firstValue index =
    firstValue
        + (toFloat index)
        * delta
        |> Round.round (getDeltaPrecision delta)
        |> String.toFloat
        |> Result.withDefault 0


toValuesFromDelta : Float -> Scale -> List Float
toValuesFromDelta delta scale =
    let
        firstValue =
            getFirstValue delta scale.bounds.lower

        tickCount =
            getCount delta scale.bounds.lower (getRange scale) firstValue
    in
        List.map (toValue delta firstValue) (List.range 0 tickCount)


toValuesFromCount : Int -> Scale -> List Float
toValuesFromCount appxCount scale =
    toValuesFromDelta (getDelta (getRange scale) appxCount) scale


toValuesAuto : Scale -> List Float
toValuesAuto =
    toValuesFromCount 10



-- Helpers


toIndexInfo : Orientation -> List Value -> List AxisLabelInfo
toIndexInfo orientation values =
    let
        lowerThanZero =
            List.length (List.filter (\v -> v < 0) values)

        hasZero =
            List.any (\v -> v == 0) values
    in
        List.indexedMap (zipWithDistance orientation hasZero lowerThanZero) values


zipWithDistance : Orientation -> Bool -> Int -> Int -> Value -> AxisLabelInfo
zipWithDistance orientation hasZero lowerThanZero index value =
    let
        distance =
            if value == 0 then
                0
            else if value > 0 && hasZero then
                index - lowerThanZero
            else if value > 0 then
                index - lowerThanZero + 1
            else
                lowerThanZero - index
    in
        { index = distance
        , value = value
        , orientation = orientation
        , text = ""
        }
