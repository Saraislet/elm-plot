module Internal.Line exposing (..)

import Svg
import Svg.Attributes
import Plot.Attributes exposing (..)
import Internal.Draw exposing (..)
import Plot.Types as Types exposing (..)


view : Plot -> Line msg -> List Point -> Svg.Svg msg
view plot config points =
    let
        instructions =
            case points of
                p1 :: rest ->
                    M p1 :: (toLinePath config.interpolation (p1 :: rest)) |> toPath plot

                _ ->
                    ""
    in
        Svg.path
            (List.append
                [ Svg.Attributes.d instructions
                , Svg.Attributes.opacity (toString config.opacity)
                , Svg.Attributes.fillOpacity "0"
                , Svg.Attributes.stroke config.stroke
                , Svg.Attributes.strokeWidth (toString config.strokeWidth ++ "px")
                , Svg.Attributes.fill "tranparent"
                , Svg.Attributes.class "elm-plot__serie--line"
                , Svg.Attributes.clipPath ("url(#" ++ toClipPathId plot ++ ")")
                ]
                config.customAttrs
            )
            []
