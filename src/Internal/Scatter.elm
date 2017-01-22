module Internal.Scatter exposing (..)

import Svg
import Svg.Attributes
import Internal.Scale exposing (..)
import Plot.Attributes exposing (..)
import Plot.Types as Types exposing (..)


view : Plot -> Scatter a -> List Point -> Svg.Svg a
view plot { fill, stroke, radius } points =
    let
        svgPoints =
            List.map (plot.scales.x.toSvgCoords) points
    in
        Svg.g
            [ Svg.Attributes.fill fill
            , Svg.Attributes.stroke stroke
            ]
            (List.map (toSvgCircle radius) svgPoints)


toSvgCircle : Int -> Point -> Svg.Svg a
toSvgCircle radius ( x, y ) =
    Svg.circle
        [ Svg.Attributes.cx (toString x)
        , Svg.Attributes.cy (toString y)
        , Svg.Attributes.r (toString radius)
        ]
        []
