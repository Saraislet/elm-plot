module Internal.Bars
    exposing
        ( view
        , toPoints
        , getYValues
        )

import Svg
import Svg.Attributes
import Plot.Attributes as Attributes exposing (..)
import Plot.Attributes as Attributes exposing (Orientation(..), MaxWidth(..))
import Plot.Types as Types exposing (..)
import Internal.Draw exposing (..)
import Internal.Scale exposing (..)
import Internal.Label as Label
import Plot.Types as Types exposing (..)


-- VIEW


view : Plot -> Bars msg -> List (BarsStyle msg) -> List Group -> Svg.Svg msg
view plot config styleConfigs groups =
    let
        width =
            toBarWidth config groups (toAutoWidth plot config styleConfigs groups)
    in
        Svg.g [] (List.map (viewGroup plot config styleConfigs width) groups)


viewGroup : Plot -> Bars msg -> List (BarsStyle msg) -> Float -> Group -> Svg.Svg msg
viewGroup plot config styleConfigs width group =
    let
        labelInfos =
            List.indexedMap
                (\i y ->
                    { index = i, xValue = group.xValue, yValue = y, text = "" }
                )
                group.yValues

        toCoords =
            toStackedCoords plot config styleConfigs width group
    in
        Svg.g []
            [ Svg.g []
                (List.map2
                    (\styleConfig info ->
                        viewBar plot width (toCoords info) config styleConfig info
                    )
                    styleConfigs
                    labelInfos
                )
            , Svg.g [ Svg.Attributes.clipPath ("url(#" ++ toClipPathId plot ++ ")") ]
                (Label.view
                    config.label
                    (\info -> placeLabel width (toCoords info))
                    labelInfos
                )
            ]


toLength : Plot -> Bars msg -> BarLabelInfo -> Value
toLength plot config bar =
    case config.stackBy of
        X ->
            toLengthTouchingXAxis plot config bar

        Y ->
            if bar.index == 0 then
                toLengthTouchingXAxis plot config bar
            else
                bar.yValue * plot.scales.y.length / (getRange plot.scales.y)


toLengthTouchingXAxis : Plot -> Bars msg -> BarLabelInfo -> Value
toLengthTouchingXAxis { scales } config { yValue, index } =
    (yValue - (clamp scales.y.bounds.lower scales.y.bounds.upper 0)) * scales.y.length / (getRange scales.y)


toXStackedOffset : List (BarsStyle msg) -> Float -> BarLabelInfo -> Value
toXStackedOffset styleConfigs width { index, yValue } =
    let
        offsetGroup =
            toFloat (List.length styleConfigs) * width / 2

        offsetBar =
            toFloat index * width
    in
        offsetBar - offsetGroup


toYStackedOffset : Group -> BarLabelInfo -> Value
toYStackedOffset { yValues } { index, yValue } =
    List.take index yValues
        |> List.filter (\y -> (y < 0) == (yValue < 0))
        |> List.sum


toStackedCoords : Plot -> Bars msg -> List (BarsStyle msg) -> Float -> Group -> BarLabelInfo -> Point
toStackedCoords plot config styleConfigs width group bar =
    case config.stackBy of
        X ->
            ( bar.xValue, max (min 0 plot.scales.y.bounds.upper) bar.yValue )
                |> toSvgCoords plot.scales
                |> addDisplacement ( toXStackedOffset styleConfigs width bar, 0 )

        Y ->
            ( bar.xValue, bar.yValue )
                |> addDisplacement ( 0, toYStackedOffset group bar )
                |> toSvgCoords plot.scales
                |> addDisplacement
                    ( -width / 2
                    , min 0 (toLength plot config bar)
                    )


placeLabel : Float -> Point -> Svg.Svg msg -> Svg.Svg msg
placeLabel width ( xSvg, ySvg ) label =
    Svg.g
        [ Svg.Attributes.transform (toTranslate ( xSvg + width / 2, ySvg ))
        , Svg.Attributes.style "text-anchor: middle;"
        ]
        [ label ]


viewBar : Plot -> Float -> Point -> Bars msg -> BarsStyle msg -> BarLabelInfo -> Svg.Svg msg
viewBar plot width ( xSvg, ySvg ) config styleConfig info =
    Svg.rect
        ([ Svg.Attributes.x (toString xSvg)
         , Svg.Attributes.y (toString ySvg)
         , Svg.Attributes.width (toString width)
         , Svg.Attributes.height (toString (abs (toLength plot config info)))
         , Svg.Attributes.fill styleConfig.fill
         , Svg.Attributes.stroke "transparent"
         ]
            ++ styleConfig.customAttrs
        )
        []



-- Calculate width


toAutoWidth : Plot -> Bars msg -> List (BarsStyle msg) -> List Group -> Float
toAutoWidth { scales } { maxWidth, stackBy } styleConfigs groups =
    let
        width =
            case stackBy of
                X ->
                    1 / toFloat (List.length styleConfigs)

                Y ->
                    1
    in
        width * scales.x.length / (getRange scales.x)


toBarWidth : Bars msg -> List Group -> Float -> Float
toBarWidth { maxWidth } groups default =
    case maxWidth of
        Attributes.Percentage perc ->
            default * (toFloat perc) / 100

        Attributes.Fixed max ->
            if default > (toFloat max) then
                toFloat max
            else
                default



-- For plot calculations


toPoints : Bars msg -> List Group -> List Point
toPoints config groups =
    List.foldl (foldPoints config) [] groups


foldPoints : Bars msg -> Group -> List Point -> List Point
foldPoints { stackBy } { xValue, yValues } points =
    if stackBy == X then
        points ++ List.map (\yValue -> ( xValue, yValue )) yValues
    else
        let
            ( positive, negative ) =
                List.partition (\y -> y >= 0) yValues
        in
            points ++ [ ( xValue, List.sum positive ), ( xValue, List.sum negative ) ]


getYValues : Value -> List Group -> Maybe (List Value)
getYValues xValue groups =
    List.map (\{ xValue, yValues } -> ( xValue, Just yValues )) groups
        |> List.filter (\( x, _ ) -> x == xValue)
        |> List.head
        |> Maybe.withDefault ( 0, Nothing )
        |> Tuple.second
