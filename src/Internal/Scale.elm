module Internal.Scale exposing (..)

import Plot.Types exposing (..)
import Plot.Attributes exposing (..)
import Internal.Types exposing (..)


unzipPoints : List Point -> Oriented (List Value)
unzipPoints points =
    List.unzip points
        |> \( x, y ) -> Oriented x y


updateValues : List Point -> Oriented Scale -> Oriented Scale
updateValues points scales =
    unzipPoints points
        |> \values ->
            { x = updateScaleValues values.x scales.x
            , y = updateScaleValues values.y scales.y
            }


updateScaleValues : List Value -> Scale -> Scale
updateScaleValues values scale =
    { scale
        | values = scale.values ++ values
        , bounds = strechBounds values scale.bounds
    }


updateBoundsArea : Oriented Scale -> Oriented Scale
updateBoundsArea scales =
    { lower = 0, upper = 0 }
        |> updateScaleBounds scales.y
        |> Oriented scales.x


updateBoundsBars : List Point -> Oriented Scale -> Oriented Scale
updateBoundsBars points scales =
    List.unzip points
        |> Tuple.first
        |> toBarXEdges
        |> updateScaleBounds scales.x
        |> \scaleX -> Oriented scaleX scales.y


toBarXEdges : List Value -> Edges
toBarXEdges xValues =
    { lower = getLowest xValues - 0.5
    , upper = getHighest xValues + 0.5
    }


updateScaleBounds : Scale -> Edges -> Scale
updateScaleBounds scale bounds =
    { scale | bounds = strechBounds [ bounds.lower, bounds.upper ] scale.bounds }


strechBounds : List Value -> Edges -> Edges
strechBounds values bounds =
    { lower = min bounds.lower (getLowest values)
    , upper = max bounds.upper (getHighest values)
    }


updateTicks : Orientation -> Oriented Scale -> List Value -> Oriented Scale
updateTicks orientation scales ticks =
    case orientation of
        X ->
            Oriented (updateScaleTicks scales.x ticks) scales.y

        Y ->
            Oriented scales.x (updateScaleTicks scales.y ticks)


updateScaleTicks : Scale -> List Value -> Scale
updateScaleTicks scale ticks =
    { scale | ticks = ticks }


scaleValue : Scale -> Value -> Value
scaleValue ({ length, bounds, offset } as scale) v =
    (v * length / (getRange scale)) + offset.lower


unScaleValue : Scale -> Value -> Value
unScaleValue ({ length, bounds, offset } as scale) v =
    ((v - offset.lower) * (getRange scale) / length) + bounds.lower


fromSvgCoordsX : Oriented Scale -> Point -> Point
fromSvgCoordsX scales ( x, y ) =
    ( unScaleValue scales.x x
    , unScaleValue scales.y (scales.y.length - y)
    )


fromSvgCoordsY : Oriented Scale -> Point -> Point
fromSvgCoordsY scales ( x, y ) =
    fromSvgCoordsX scales ( y, x )


toSvgCoordsX : Oriented Scale -> Point -> Point
toSvgCoordsX scales ( x, y ) =
    ( scaleValue scales.x (x - scales.x.bounds.lower)
    , scaleValue scales.y (scales.y.bounds.upper - y)
    )


toSvgCoordsY : Oriented Scale -> Point -> Point
toSvgCoordsY scales ( x, y ) =
    toSvgCoordsX scales ( y, x )


getHighest : List Float -> Float
getHighest values =
    Maybe.withDefault 10 (List.maximum values)


getLowest : List Float -> Float
getLowest values =
    Maybe.withDefault 0 (List.minimum values)


getRange : Scale -> Float
getRange { bounds } =
    if (bounds.upper - bounds.lower) > 0 then
        bounds.upper - bounds.lower
    else
        1


getEdgesX : List Point -> ( Float, Float )
getEdgesX points =
    getEdges <| List.map Tuple.first points


getEdgesY : List Point -> ( Float, Float )
getEdgesY points =
    getEdges <| List.map Tuple.second points


getEdges : List Float -> ( Float, Float )
getEdges range =
    ( getLowest range, getHighest range )


pixelsToValue : Float -> Float -> Float -> Float
pixelsToValue length range pixels =
    range * pixels / length


ceilToNearest : Float -> Float -> Float
ceilToNearest precision value =
    toFloat (ceiling (value / precision)) * precision


getDifference : Float -> Float -> Float
getDifference a b =
    abs <| a - b


getClosest : Float -> Float -> Maybe Float -> Maybe Float
getClosest value candidate closest =
    case closest of
        Just closeValue ->
            if getDifference value candidate < getDifference value closeValue then
                Just candidate
            else
                Just closeValue

        Nothing ->
            Just candidate


toNearestX : Oriented Scale -> Float -> Maybe Float
toNearestX scales value =
    List.foldr (getClosest value) Nothing scales.x.values


postProcessPlot : Plot -> Plot
postProcessPlot plot =
    { plot | scales = postProcessScales plot.scales }
        |> postProcessTranslator


postProcessTranslator : Plot -> Plot
postProcessTranslator plot =
    { plot | scales = postProcessScaleTranslator plot.scales }


postProcessScaleTranslator : Oriented Scale -> Oriented Scale
postProcessScaleTranslator ({ x, y } as scales) =
    { x = { x | toSvgCoords = toSvgCoordsX scales, fromSvgCoords = fromSvgCoordsX scales }
    , y = { y | toSvgCoords = toSvgCoordsY scales, fromSvgCoords = fromSvgCoordsY scales }
    }


postProcessScales : Oriented Scale -> Oriented Scale
postProcessScales { x, y } =
    { x = { x | length = getInnerLength x }
    , y = { y | length = getInnerLength y }
    }


getInnerLength : Scale -> Float
getInnerLength scale =
    scale.lengthTotal - scale.offset.lower - scale.offset.upper


flipPlotToOrientation : Orientation -> Plot -> Plot
flipPlotToOrientation orientation plot =
    case orientation of
        X ->
            plot

        Y ->
            { plot | scales = { x = plot.scales.y, y = plot.scales.x } }
