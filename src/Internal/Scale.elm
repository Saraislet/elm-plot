module Internal.Scale exposing (..)

import Plot.Types exposing (..)
import Plot.Attributes exposing (..)
import Internal.Types exposing (..)


-- Finding the bounds of the plot


toBounds : List Value -> Edges
toBounds values =
    { lower = getLowest values
    , upper = getHighest values
    }


foldBounds : Oriented Edges -> Oriented Edges -> Oriented Edges
foldBounds newBounds oldBounds =
    { x = foldSingleBounds oldBounds.x newBounds.x
    , y = foldSingleBounds oldBounds.y newBounds.y
    }


foldSingleBounds : Edges -> Edges -> Edges
foldSingleBounds boundsA boundsB =
    { lower = min boundsA.lower boundsB.lower
    , upper = max boundsA.upper boundsB.upper
    }


{-| Finds the bounds from a list of points.
-}
updateBoundsFromPoints : List Point -> Oriented Edges
updateBoundsFromPoints points =
    List.unzip points |> \( xValues, yValues ) -> Oriented (toBounds xValues) (toBounds yValues)


{-| The area serie bounds defaults to begin the yAxis at minimum 0.
-}
updateBoundsArea : List Point -> Oriented Edges
updateBoundsArea points =
    updateBoundsFromPoints points
        |> addAreaBoundsPadding


addAreaBoundsPadding : Oriented Edges -> Oriented Edges
addAreaBoundsPadding bounds =
    Oriented bounds.x { lower = min 0 bounds.y.lower, upper = bounds.y.upper }


{-| The bars serie bounds adds 0.5 to each side of the xAxis as the bars are
 centered and we don't want to cut it off.
-}
updateBoundsBars : List Point -> Oriented Edges
updateBoundsBars points =
    updateBoundsFromPoints points
        |> addBarsBoundsPadding


addBarsBoundsPadding : Oriented Edges -> Oriented Edges
addBarsBoundsPadding bounds =
    Oriented { lower = bounds.x.lower - 0.5, upper = bounds.x.upper + 0.5 } bounds.y


updateTicks : Orientation -> Oriented Scale -> List Value -> Oriented Scale
updateTicks orientation scales ticks =
    case orientation of
        X ->
            Oriented (updateScaleTicks scales.x ticks) scales.y

        Y ->
            Oriented scales.x (updateScaleTicks scales.y ticks)


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
    List.foldr (getClosest value) Nothing []


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


flipPlotToOrientation : Orientation -> Plot -> Plot
flipPlotToOrientation orientation plot =
    case orientation of
        X ->
            plot

        Y ->
            { plot | scales = { x = plot.scales.y, y = plot.scales.x } }


updateScaleTicks : Scale -> List Value -> Scale
updateScaleTicks scale ticks =
    { scale | ticks = ticks }



-- Scalers


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



-- Helpers


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


getInnerLength : Scale -> Float
getInnerLength scale =
    scale.lengthTotal - scale.offset.lower - scale.offset.upper
