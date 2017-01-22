module Internal.Scale exposing (..)

import Plot.Types exposing (..)


type alias Edges =
    { lower : Float
    , upper : Float
    }


type alias Oriented a =
    { x : a
    , y : a
    }


type alias Scale =
    { values : List Float
    , ticks : List Float
    , bounds : Edges
    , padding : Edges
    , offset : Edges
    , length : Float
    }


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


updateScaleTicks : Scale -> List Value -> Scale
updateScaleTicks scale ticks =
    { scale | ticks = ticks }


scaleValue : Scale -> Value -> Value
scaleValue ({ length, bounds, offset } as scale) v =
    (v * length / (getRange scale)) + offset.lower


unScaleValue : Scale -> Value -> Value
unScaleValue ({ length, bounds, offset } as scale) v =
    ((v - offset.lower) * (getRange scale) / length) + bounds.lower


fromSvgCoords : Oriented Scale -> Point -> Point
fromSvgCoords scales ( x, y ) =
    ( unScaleValue scales.x x
    , unScaleValue scales.y (scales.y.length - y)
    )


toSvgCoords : Oriented Scale -> Point -> Point
toSvgCoords scales ( x, y ) =
    ( scaleValue scales.x (x - scales.x.bounds.lower)
    , scaleValue scales.y (scales.y.bounds.upper - y)
    )


toSvgCoordsFlipped : Oriented Scale -> Point -> Point
toSvgCoordsFlipped scales ( x, y ) =
    ( scaleValue scales.y (y - scales.y.bounds.lower)
    , scaleValue scales.x (scales.x.bounds.upper - x)
    )


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


foldBounds : Maybe Edges -> Edges -> Edges
foldBounds oldBounds newBounds =
    case oldBounds of
        Just bounds ->
            { lower = min bounds.lower newBounds.lower
            , upper = max bounds.upper newBounds.upper
            }

        Nothing ->
            newBounds


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


updatePadding : ( Int, Int ) -> Scale -> Scale
updatePadding ( bottom, top ) scale =
    { scale | padding = Edges (toFloat bottom) (toFloat top) }


updateLength : Int -> Scale -> Scale
updateLength length scale =
    { scale | length = toFloat length }


updateOffset : Int -> Int -> Scale -> Scale
updateOffset lower upper scale =
    { scale | offset = Edges (toFloat lower) (toFloat upper) }


applyBounds : (Float -> Float) -> (Float -> Float) -> Scale -> Scale
applyBounds toLower toUpper scale =
    { scale | offset = Edges (toLower scale.bounds.lower) (toUpper scale.bounds.lower) }
