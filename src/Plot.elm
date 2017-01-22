module Plot
    exposing
        ( plot
        , plotInteractive
        , verticalGrid
        , horizontalGrid
        , hint
        , area
        , line
        , bars
        , scatter
        , custom
        , Element
        , initialState
        , update
        , Interaction(..)
        , State
        , getHoveredValue
        )

{-|
 This library aims to allow you to visualize a variety of graphs in
 an intuitive manner without compromising flexibility regarding configuration.
 It is inspired by the elm-html api, using the `element attrs children` pattern.

 This is still in beta! The api might and probably will change!

 Just FYI, [`Svg msg`](http://package.elm-lang.org/packages/elm-lang/svg/2.0.0/Svg#Svg)
 is an alias to `VirtualDom.Node msg` and so is [`Html msg`](http://package.elm-lang.org/packages/elm-lang/html/2.0.0/Html#Html).
 This means that the types are basically interchangable and you can use the same methods on them.

# Definitions
@docs  Element

# Elements
@docs plot, plotInteractive, hint, verticalGrid, horizontalGrid, custom

## Series
@docs scatter, line, area, bars

# Styling and sizes
@docs id, classes, margin, padding, size, style, domainLowest, domainHighest, rangeLowest, rangeHighest

# State
For an example of the update flow see [this example](https://github.com/terezka/elm-plot/blob/master/examples/Interactive.elm).

@docs State, initialState, update, Interaction, getHoveredValue


-}

import Html exposing (Html)
import Html.Attributes
import Html.Events
import Svg exposing (Svg)
import Svg.Attributes
import Svg.Lazy
import Json.Decode as Json
import DOM
import Plot.Bars as Bars
import Internal.Grid as GridInternal
import Internal.Bars as BarsInternal
import Internal.Area as AreaInternal
import Internal.Scatter as ScatterInternal
import Internal.Line as LineInternal
import Internal.Hint as HintInternal
import Internal.Types exposing (..)
import Internal.Draw exposing (..)
import Internal.Scale exposing (..)
import Plot.Attributes as Attributes exposing (..)


{-| Represents a child element of the plot.
-}
type Element msg
    = Line (Line msg) (List Point)
    | Area (Area msg) (List Point)
    | Bars (Bars msg) (List (BarsStyle msg)) (List Bars.Data)
    | Scatter (Scatter msg) (List Point)
    | Hint (Hint msg) (Maybe Point)
    | Grid (Grid msg)
    | CustomElement ((Point -> Point) -> Svg.Svg msg)


{-| -}
horizontalGrid : List (Attribute (Grid msg)) -> Element msg
horizontalGrid attrs =
    Grid (List.foldr (<|) defaultGridConfig attrs)


{-| -}
verticalGrid : List (Attribute (Grid msg)) -> Element msg
verticalGrid attrs =
    Grid (List.foldr (<|) { defaultGridConfig | orientation = Y } attrs)


{-| -}
area : List (Attribute (Area msg)) -> List Point -> Element msg
area attrs points =
    Area (List.foldr (<|) defaultArea attrs) points


{-| -}
line : List (Attribute (Line msg)) -> List Point -> Element msg
line attrs points =
    Line (List.foldr (<|) defaultLine attrs) points


{-| -}
scatter : List (Attribute (Scatter msg)) -> List Point -> Element msg
scatter attrs =
    Scatter (List.foldr (<|) defaultScatterConfig attrs)


{-| This wraps all your bar series.
-}
bars : List (Attribute (Bars msg)) -> List (List (Attribute (BarsStyle msg))) -> List Bars.Data -> Element msg
bars attrs styleAttrsList groups =
    Bars
        (List.foldr (<|) defaultBarsConfig attrs)
        (List.map (List.foldr (<|) defaultBarsStyle) styleAttrsList)
        groups


{-| Adds a hint to your plot. See [this example](https://github.com/terezka/elm-plot/blob/master/examples/Interactive.elm)

 Remember to use `plotInteractive` for the events to be processed!.
-}
hint : List (Attribute (Hint msg)) -> Maybe Point -> Element msg
hint attrs position =
    Hint (List.foldr (<|) defaultHintConfig attrs) position


{-| This element is passed a function which can translate your values into
 svg coordinates. This way you can build your own serie types. Although
 if you feel like you're missing something let me know!
-}
custom : ((Point -> Point) -> Svg.Svg msg) -> Element msg
custom view =
    CustomElement view


{-| This is the function processing your entire plot configuration.
 Pass your attributes and elements to this function and
 a SVG plot will be returned!
-}
plot : List (Attribute Plot) -> List (Element msg) -> Svg msg
plot attrs elements =
    Svg.Lazy.lazy2 parsePlot (toPlotConfig attrs elements) elements


{-| So this is like `plot`, except the message to is `Interaction msg`. It's a message wrapping
 your message, so you can use the build in interactions (like the hint!) in the plot as well as adding your own.
 See [this example](https://github.com/terezka/elm-plot/blob/master/examples/Interactive.elm).
-}
plotInteractive : List (Attribute Plot) -> List (Element (Interaction msg)) -> Svg (Interaction msg)
plotInteractive attrs elements =
    Svg.Lazy.lazy2 parsePlotInteractive (toPlotConfig attrs elements) elements


toPlotConfig : List (Attribute Plot) -> List (Element msg) -> Plot
toPlotConfig attributes elements =
    defaultConfig
        |> applyElements elements
        |> applyAttributes attributes



-- MODEL


{-| -}
type State
    = State StateInner


type alias StateInner =
    { position : Maybe ( Float, Float ) }


{-| -}
initialState : State
initialState =
    State { position = Nothing }



-- UPDATE


{-| -}
type Interaction msg
    = Internal Msg
    | Custom msg


type Msg
    = Hovering ( Float, Float )
    | ResetPosition


{-| -}
update : Msg -> State -> State
update msg (State state) =
    case msg of
        Hovering position ->
            if shouldPositionUpdate state position then
                State { state | position = Just position }
            else
                State state

        ResetPosition ->
            State { position = Nothing }


{-| Get the hovered position from state.
-}
getHoveredValue : State -> Maybe Point
getHoveredValue (State { position }) =
    position


shouldPositionUpdate : StateInner -> ( Float, Float ) -> Bool
shouldPositionUpdate { position } ( left, top ) =
    case position of
        Nothing ->
            True

        Just ( leftOld, topOld ) ->
            topOld /= top || leftOld /= left


getRelativePosition : Plot -> ( Float, Float ) -> ( Float, Float ) -> ( Maybe Float, Float )
getRelativePosition plot ( mouseX, mouseY ) ( left, top ) =
    let
        ( x, y ) =
            fromSvgCoords plot ( mouseX - left, mouseY - top )
    in
        ( toNearestX plot x, y )


handleMouseOver : Plot -> Json.Decoder (Interaction msg)
handleMouseOver plot =
    Json.map3
        (toMouseOverMsg plot)
        (Json.field "clientX" Json.float)
        (Json.field "clientY" Json.float)
        (DOM.target getPlotPosition)


toMouseOverMsg : Plot -> Float -> Float -> ( Float, Float ) -> Interaction msg
toMouseOverMsg plot mouseX mouseY position =
    let
        relativePosition =
            getRelativePosition plot ( mouseX, mouseY ) position
    in
        case Tuple.first relativePosition of
            Just x ->
                Internal (Hovering ( x, Tuple.second relativePosition ))

            Nothing ->
                Internal ResetPosition


getPlotPosition : Json.Decoder ( Float, Float )
getPlotPosition =
    Json.oneOf
        [ getPosition
        , Json.lazy (\_ -> getParentPosition)
        ]


getPosition : Json.Decoder ( Float, Float )
getPosition =
    Json.map (\{ left, top } -> ( left, top )) DOM.boundingClientRect


getParentPosition : Json.Decoder ( Float, Float )
getParentPosition =
    DOM.parentElement getPlotPosition



-- VIEW


parsePlot : Plot -> List (Element msg) -> Svg msg
parsePlot plot elements =
    viewPlot plot (viewElements plot elements)


parsePlotInteractive : Plot -> List (Element (Interaction msg)) -> Svg (Interaction msg)
parsePlotInteractive config elements =
    viewPlotInteractive plot (viewElements plot elements)


viewPlotInteractive : Plot -> ( List (Svg (Interaction msg)), List (Html (Interaction msg)) ) -> Html (Interaction msg)
viewPlotInteractive plot ( svgViews, htmlViews ) =
    Html.div
        (plotAttributes plot ++ plotAttributesInteraction plot)
        (viewSvg plot svgViews :: htmlViews)


viewPlot : Plot -> ( List (Svg msg), List (Html msg) ) -> Svg msg
viewPlot plot ( svgViews, htmlViews ) =
    Html.div
        (plotAttributes plot)
        (viewSvg plot svgViews :: htmlViews)


plotAttributes : Plot -> List (Html.Attribute msg)
plotAttributes { scales, style, id } =
    [ Html.Attributes.class "elm-plot"
    , Html.Attributes.id id
    , Html.Attributes.style <| sizeStyle scales ++ style
    ]


plotAttributesInteraction : Plot -> List (Html.Attribute (Interaction msg))
plotAttributesInteraction plot =
    [ Html.Events.on "mousemove" (handleMouseOver plot)
    , Html.Events.onMouseLeave (Internal ResetPosition)
    ]


viewSvg : Plot -> List (Svg msg) -> Svg msg
viewSvg plot views =
    Svg.svg
        [ Svg.Attributes.height (toString plot.scales.y.length)
        , Svg.Attributes.width (toString plot.scales.x.length)
        , Svg.Attributes.class "elm-plot__inner"
        ]
        (scaleDefs plot :: views)


scaleDefs : Plot -> Svg.Svg msg
scaleDefs plot =
    Svg.defs []
        [ Svg.clipPath [ Svg.Attributes.id (toClipPathId plot) ]
            [ Svg.rect
                [ Svg.Attributes.x (toString plot.scale.x.offset.lower)
                , Svg.Attributes.y (toString plot.scale.y.offset.lower)
                , Svg.Attributes.width (toString plot.scale.x.length)
                , Svg.Attributes.height (toString plot.scale.y.length)
                ]
                []
            ]
        ]


sizeStyle : Oriented Scale -> Style
sizeStyle scales =
    [ ( "height", toPixels scales.y.lenght ), ( "width", toPixels scales.x.lenght ) ]


viewElements : Plot -> List (Element msg) -> ( List (Svg msg), List (Html msg) )
viewElements plot elements =
    List.foldr (viewElement plot) ( [], [] ) elements


viewElement : Plot -> Element msg -> ( List (Svg msg), List (Html msg) ) -> ( List (Svg msg), List (Html msg) )
viewElement plot element ( svgViews, htmlViews ) =
    ( [], [] )



-- CALCULATIONS OF META


applyElements : List (Element msg) -> Plot -> Plot
applyElements elements plot =
    { plot
        | scales = List.foldl toScales elements plot.scales
        , getHintInfo = getHintInfo elements
    }


applyAttributes : List (Attribute Plot) -> Plot -> Plot
applyAttributes =
    List.foldl (|>)


toScales : Element msg -> Oriented Scale -> Oriented Scale
toScales element scales =
    case element of
        Line _ points ->
            updateValues points scales

        Area _ points ->
            updateValues points scales
                |> updateBoundsArea

        Scatter _ points ->
            updateValues points scales

        Bars config _ groups ->
            let
                points =
                    BarsInternal.toPoints config groups
            in
                updateValues points scales
                    |> updateBoundsBars points

        _ ->
            scales


getHintInfo : List (Element msg) -> Float -> HintInfo
getHintInfo elements xValue =
    { xValue = xValue
    , yValues = List.foldr (collectYValues xValue) [] elements
    , isLeftSide = True
    }


collectYValues : Float -> Element msg -> List (Maybe (List Value)) -> List (Maybe (List Value))
collectYValues xValue element yValues =
    case element of
        Area config points ->
            collectYValue xValue points :: yValues

        Line config points ->
            collectYValue xValue points :: yValues

        Scatter config points ->
            collectYValue xValue points :: yValues

        Bars config styleConfigs groups ->
            BarsInternal.getYValues xValue groups :: yValues

        _ ->
            yValues


collectYValue : Float -> List Point -> Maybe (List Value)
collectYValue xValue points =
    List.foldr (getYValue xValue) Nothing points


getYValue : Float -> Point -> Maybe (List Value) -> Maybe (List Value)
getYValue xValue ( x, y ) result =
    if x == xValue then
        Just [ y ]
    else
        result
