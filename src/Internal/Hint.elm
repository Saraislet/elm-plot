module Internal.Hint exposing (..)

import Svg
import Internal.Scale exposing (..)
import Internal.Draw exposing (..)
import Internal.Line as Line
import Html
import Html.Attributes
import Plot.Attributes exposing (Orientation(..))
import Plot.Attributes exposing (..)
import Plot.Types as Types exposing (..)


view : Plot -> Hint msg -> ( Float, Float ) -> ( Svg.Svg msg, Html.Html msg )
view plot { lineStyle, view } position =
    let
        xPosition =
            Tuple.first position

        info =
            plot.getHintInfo xPosition

        viewHint =
            Maybe.withDefault defaultView view

        ( xSvg, ySvg ) =
            plot.scales.x.toSvgCoords ( info.xValue, 0 )

        isLeftSide =
            xSvg - plot.scales.x.offset.lower < plot.scales.x.length / 2

        infoWithPosition =
            { info | isLeftSide = isLeftSide }

        lineView =
            Line.view plot
                lineStyle
                [ ( xPosition, plot.scales.y.bounds.upper )
                , ( xPosition, plot.scales.y.bounds.lower )
                ]
    in
        ( lineView
        , Html.div
            [ Html.Attributes.class "elm-plot__hint"
            , Html.Attributes.style
                [ ( "left", toPixels xSvg )
                , ( "top", toPixels plot.scales.y.offset.lower )
                , ( "position", "absolute" )
                , ( "height", "100%" )
                ]
            ]
            [ viewHint infoWithPosition ]
        )


defaultView : HintInfo -> Html.Html msg
defaultView { xValue, yValues, isLeftSide } =
    let
        classes =
            [ ( "elm-plot__hint__default-view", True )
            , ( "elm-plot__hint__default-view--left", isLeftSide )
            , ( "elm-plot__hint__default-view--right", not isLeftSide )
            ]
    in
        Html.div
            [ Html.Attributes.classList classes ]
            [ Html.div [] [ Html.text ("X: " ++ toString xValue) ]
            , Html.div [] (List.indexedMap viewYValue yValues)
            ]


viewYValue : Int -> Maybe (List Value) -> Html.Html msg
viewYValue index hintValue =
    let
        hintValueDisplayed =
            case hintValue of
                Just value ->
                    case value of
                        [ singleY ] ->
                            toString singleY

                        _ ->
                            toString value

                Nothing ->
                    "~"
    in
        Html.div []
            [ Html.span [] [ Html.text ("Serie " ++ toString index ++ ": ") ]
            , Html.span [] [ Html.text hintValueDisplayed ]
            ]
