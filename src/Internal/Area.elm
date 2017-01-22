module Internal.Area exposing (..)

import Svg
import Svg.Attributes
import Internal.Scale exposing (getEdgesX, toSvgCoords)
import Internal.Draw exposing (PathType(..), toPath, toLinePath, toStyle, toClipPathId)
import Plot.Attributes exposing (Plot, Area, InterpolationOption(..))
import Plot.Types as Types exposing (..)


view : Plot -> Area a -> List Point -> Svg.Svg a
view plot config points =
    let
        ( lowestX, highestX ) =
            getEdgesX points

        lowestY =
            clamp plot.scales.y.bounds.lower plot.scales.y.bounds.upper 0

        instructions =
            List.concat
                [ [ M ( lowestX, lowestY ) ]
                , (toLinePath config.interpolation points)
                , [ L ( highestX, lowestY ), Z ]
                ]
                |> toPath plot
    in
        Svg.path
            (List.append
                [ Svg.Attributes.d instructions
                , Svg.Attributes.opacity (toString config.opacity)
                , Svg.Attributes.fill config.fill
                , Svg.Attributes.strokeWidth (toString config.strokeWidth ++ "px")
                , Svg.Attributes.class "elm-plot__serie--area"
                , Svg.Attributes.clipPath ("url(#" ++ toClipPathId plot ++ ")")
                ]
                config.customAttrs
            )
            []
