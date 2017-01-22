module Plot.Attributes
    exposing
        ( Attribute
        , stroke
        , strokeWidth
        , fill
        , opacity
        , classes
        , displace
        , lineStyle
        , InterpolationOption(..)
        , interpolation
        , maxBarWidthPer
        , label
        , radius
        , tick
        , FormatOption(..)
        , format
        , ValuesOption(..)
        , values
        , AnchorOption(..)
        , anchor
        , PositionOption(..)
        , position
        , clearIntersections
        , defaultLine
        , Tick
        , TickStyle
        , defaultTickStyle
        , defaultTickConfig
        , Line
        , Label
        , LabelStyle
        , defaultLabelStyle
        , Area
        , defaultArea
        , Grid
        , defaultGridConfig
        , Group
        , Bars
        , BarsStyle
        , BarLabelInfo
        , Axis
        , defaultAxisConfig
        , AxisLabelInfo
        , MaxWidth(..)
        , defaultBarsConfig
        , defaultBarsStyle
        , length
        , viewStatic
        , viewDynamic
        , viewCustom
        , Scatter
        , defaultScatterConfig
        , stackByY
        , maxBarWidth
        , fontSize
        , Hint
        , defaultHintConfig
        , HintInfo
        , Orientation(..)
        , Plot
        , defaultConfig
        , margin
        , padding
        , id
        , size
        , style
        , domainLowest
        , domainHighest
        , rangeLowest
        , rangeHighest
        )

{-| Attributes to alter styling.
-}

import Svg
import Html
import Internal.View as View
import Internal.Scale as Scale exposing (..)
import Plot.Types as Types exposing (..)


{-| -}
type alias Attribute a =
    a -> a


{-| -}
type Orientation
    = X
    | Y


type alias Plot =
    { id : String
    , classes : List String
    , style : Style
    , getHintInfo : Value -> HintInfo
    , scales : Oriented Scale
    }


defaultConfig : Plot
defaultConfig =
    { id = "elm-plot"
    , classes = []
    , style = []
    , getHintInfo = always { xValue = 0, yValues = [], isLeftSide = True }
    , scales = Oriented (defaultScale 800) (defaultScale 500)
    }


defaultScale : Float -> Scale
defaultScale length =
    { bounds = { lower = 0, upper = 1 }
    , offset = { lower = 0, upper = 0 }
    , padding = { lower = 0, upper = 0 }
    , ticks = []
    , length = length
    , values = []
    }


{-| Adds padding to your plot, meaning extra space below
 and above the lowest and highest point in your plot.
 The unit is pixels and the format is `( bottom, top )`.

 Default: `( 0, 0 )`
-}
padding : ( Int, Int ) -> Attribute Plot
padding padding config =
    { config | scales = Oriented config.scales.x (updatePadding padding config.scales.y) }


{-| Specify the size of your plot in pixels
-}
size : ( Int, Int ) -> Attribute Plot
size ( width, height ) config =
    { config | scales = Oriented (updateLength width config.scales.x) (updateLength height config.scales.y) }


{-| Specify margin around the plot. Useful when your ticks are outside the
 plot and you would like to add space to see them! Values are in pixels and
the format is `( top, right, bottom, left )`.

 Default: `( 0, 0, 0, 0 )`
-}
margin : ( Int, Int, Int, Int ) -> Attribute Plot
margin ( t, r, b, l ) config =
    { config | scales = Oriented (updateOffset l r config.scales.x) (updateOffset b t config.scales.y) }


{-| Adds styles to the svg element.
-}
style : Style -> Attribute { a | style : Style }
style style config =
    { config | style = defaultConfig.style ++ style ++ [ ( "padding", "0" ) ] }


{-| Adds an id to the svg element.
-}
id : String -> Attribute Plot
id id config =
    { config | id = id }


{-| Alter the domain's lower boundary. The function provided will
 be passed the lowest y-value present in any of your series and the result will
 be the lower boundary of your series. So if you would like
 the lowest boundary to simply be the edge of your series, then set
 this attribute to the function `identity`.
 If you want it to always be -5, then set this attribute to the function `always -5`.

 The default is `identity`.

 **Note:** If you are using `padding` as well, the extra padding will still be
 added outside the domain.
-}
domainLowest : (Float -> Float) -> Attribute Plot
domainLowest toLowest config =
    { config | scales = Oriented config.scales.x (applyBounds toLowest identity config.scales.y) }


{-| Alter the domain's upper boundary. The function provided will
 be passed the lowest y-value present in any of your series and the result will
 be the upper boundary of your series. So if you would like
 the lowest boundary to  always be 10, then set this attribute to the function `always 10`.

 The default is `identity`.

 **Note:** If you are using `padding` as well, the extra padding will still be
 added outside the domain.
-}
domainHighest : (Float -> Float) -> Attribute Plot
domainHighest toHighest config =
    { config | scales = Oriented config.scales.x (applyBounds identity toHighest config.scales.y) }


{-| Provide a function to determine the lower boundary of range.
 See `domainLowest` and imagine we're talking about the x-axis.
-}
rangeLowest : (Float -> Float) -> Attribute Plot
rangeLowest toLowest config =
    { config | scales = Oriented (applyBounds toLowest identity config.scales.y) config.scales.y }


{-| Provide a function to determine the upper boundary of range.
 See `domainHighest` and imagine we're talking about the x-axis.
-}
rangeHighest : (Float -> Float) -> Attribute Plot
rangeHighest toHighest config =
    { config | scales = Oriented (applyBounds identity toHighest config.scales.x) config.scales.y }


{-| Set the stroke color.
-}
stroke : String -> Attribute { c | stroke : String }
stroke stroke config =
    { config | stroke = stroke }


{-| Set the stroke width (in pixels).
-}
strokeWidth : Int -> Attribute { c | strokeWidth : Int }
strokeWidth strokeWidth config =
    { config | strokeWidth = strokeWidth }


{-| Set the fill color.
-}
fill : String -> Attribute { c | fill : String }
fill fill config =
    { config | fill = fill }


{-| Set the opacity.
-}
opacity : Float -> Attribute { c | opacity : Float }
opacity opacity config =
    { config | opacity = opacity }


{-| Displace an element.
-}
displace : ( Int, Int ) -> Attribute { c | displace : ( Int, Int ) }
displace displace config =
    { config | displace = displace }


{-| Set the font size (in pixels).
-}
fontSize : Int -> Attribute { c | style : Style }
fontSize fontSize config =
    { config | style = ( "font-size", toString fontSize ++ "px" ) :: config.style }


type InterpolationOption
    = Bezier
    | NoInterpolation


{-| Smooth line with [Bézier curves] (https://en.wikipedia.org/wiki/B%C3%A9zier_curve).
-}
interpolation : InterpolationOption -> Attribute { c | interpolation : InterpolationOption }
interpolation interpolation config =
    { config | interpolation = interpolation }


{-| Add your own attributes. For events, see [this example](https://github.com/terezka/elm-plot/blob/master/examples/Interactive.elm)
-}
customAttrs : List (Svg.Attribute msg) -> Attribute { c | customAttrs : List (Svg.Attribute msg) }
customAttrs attrs config =
    { config | customAttrs = attrs }


{-| Adds classes.
-}
classes : List String -> Attribute { c | classes : List String }
classes classes config =
    { config | classes = classes }


{-| Adds classes.
-}
length : Int -> Attribute { c | length : Int }
length length config =
    { config | length = length }


{-| Adds classes.
-}
width2 : Int -> Attribute { c | width : Int }
width2 width config =
    { config | width = width }


{-| Set the radius of your points.
-}
radius : Int -> Attribute { c | radius : Int }
radius radius config =
    { config | radius = radius }


{-| Set HTML view. (Like for the hint)
-}
viewHtml : (a -> Html.Html msg) -> Attribute { c | view : a -> Html.Html msg }
viewHtml view config =
    { config | view = view }



-- Highlevel attributes


{-| The available info provided to your hint view.
-}
type alias HintInfo =
    { xValue : Float
    , yValues : List (Maybe (List Value))
    , isLeftSide : Bool
    }


type alias Hint msg =
    { view : Maybe (HintInfo -> Html.Html msg)
    , lineStyle : Line msg
    }


defaultHintConfig : Hint msg
defaultHintConfig =
    { view = Nothing
    , lineStyle = defaultLine
    }


type alias Scatter a =
    { radius : Int
    , fill : String
    , stroke : String
    , customAttrs : List (Svg.Attribute a)
    , radius : Int
    }


defaultScatterConfig : Scatter a
defaultScatterConfig =
    { stroke = "black"
    , fill = "transparent"
    , customAttrs = []
    , radius = 5
    }


type alias AxisLabelInfo =
    { value : Float
    , index : Int
    , orientation : Orientation
    , text : String
    }


type alias Axis msg =
    { tick : Tick AxisLabelInfo msg
    , label : Label { values : ValuesOption } AxisLabelInfo msg
    , lineStyle : Line msg
    , orientation : Orientation
    , anchor : AnchorOption
    , clearIntersections : Bool
    , position : PositionOption
    , classes : List String
    }


defaultAxisConfig : Axis msg
defaultAxisConfig =
    { tick = defaultTickConfig
    , label =
        { view = View.FromConfig defaultLabelStyle
        , format = FormatFromFunc (.value >> toString)
        , values = ValuesAuto
        }
    , lineStyle = defaultLine
    , orientation = X
    , clearIntersections = False
    , anchor = AnchorOuter
    , position = PositionAtZero
    , classes = []
    }


type alias BarLabelInfo =
    { index : Int
    , xValue : Value
    , yValue : Value
    , text : String
    }


type alias Group =
    { xValue : Value
    , yValues : List Value
    }


type alias BarsStyle msg =
    { fill : String
    , customAttrs : List (Svg.Attribute msg)
    }


type alias Bars msg =
    { stackBy : Orientation
    , label : Label {} BarLabelInfo msg
    , maxWidth : MaxWidth
    }


defaultBarsConfig : Bars msg
defaultBarsConfig =
    { stackBy = X
    , label =
        { view = View.FromConfig defaultLabelStyle
        , format = FormatFromFunc (always "")
        }
    , maxWidth = Percentage 100
    }


defaultBarsStyle : BarsStyle msg
defaultBarsStyle =
    { fill = "grey"
    , customAttrs = []
    }


type alias Area a =
    { stroke : String
    , strokeWidth : Int
    , fill : String
    , opacity : Float
    , interpolation : InterpolationOption
    , customAttrs : List (Svg.Attribute a)
    }


defaultArea : Area a
defaultArea =
    { stroke = "black"
    , strokeWidth = 1
    , fill = "grey"
    , opacity = 1
    , interpolation = NoInterpolation
    , customAttrs = []
    }


type alias Grid a =
    { values : ValuesOption
    , lineStyle : Line a
    , classes : List String
    , orientation : Orientation
    , customAttrs : List (Svg.Attribute a)
    }


defaultGridConfig : Grid a
defaultGridConfig =
    { values = ValuesAuto
    , lineStyle = defaultLine
    , classes = []
    , orientation = X
    , customAttrs = []
    }


type alias Line a =
    { stroke : String
    , strokeWidth : Int
    , opacity : Float
    , interpolation : InterpolationOption
    , customAttrs : List (Svg.Attribute a)
    }


defaultLine : Line a
defaultLine =
    { stroke = "black"
    , strokeWidth = 1
    , opacity = 1
    , interpolation = NoInterpolation
    , customAttrs = []
    }


{-| Configures a line.
-}
lineStyle : List (Attribute (Line msg)) -> Attribute { a | lineStyle : Line msg }
lineStyle attrs config =
    { config | lineStyle = List.foldr (<|) defaultLine attrs }



--- Label


type alias Label c a msg =
    { c | view : View.ViewOption a (LabelStyle msg) msg, format : FormatOption a }


type alias LabelStyle msg =
    { displace : ( Int, Int )
    , style : Style
    , opacity : Int
    , fill : String
    , strokeWidth : Int
    , stroke : String
    , classes : List String
    , customAttrs : List (Svg.Attribute msg)
    }


defaultLabelStyle : LabelStyle msg
defaultLabelStyle =
    { displace = ( 0, 0 )
    , style = []
    , opacity = 1
    , strokeWidth = 1
    , fill = "grey"
    , stroke = "black"
    , classes = []
    , customAttrs = []
    }


{-| By providing this attribute with a list of [label attributes](http://package.elm-lang.org/packages/terezka/elm-plot/latest/Plot-Label),
 you may alter the values and ticks displayed as your axis' labels.
-}
label : List (Attribute (Label c v msg)) -> Attribute { a | label : Label c v msg }
label attributes config =
    { config | label = List.foldl (<|) config.label attributes }



-- Ticks


type alias Tick a msg =
    { view : View.ViewOption a (TickStyle msg) msg
    , values : ValuesOption
    }


type alias TickStyle msg =
    { length : Int
    , stroke : String
    , width : Int
    , style : Style
    , classes : List String
    , customAttrs : List (Svg.Attribute msg)
    }


defaultTickConfig : Tick a msg
defaultTickConfig =
    { view = View.FromConfig defaultTickStyle
    , values = ValuesAuto
    }


defaultTickStyle : TickStyle msg
defaultTickStyle =
    { length = 7
    , stroke = "black"
    , width = 1
    , style = []
    , classes = []
    , customAttrs = []
    }


{-| By providing this attribute with a list of [tick attributes](http://package.elm-lang.org/packages/terezka/elm-plot/latest/Plot-Tick),
 you may alter the values and ticks displayed as your axis' ticks.
-}
tick : List (Attribute (Tick v msg)) -> Attribute { a | tick : Tick v msg }
tick attributes config =
    { config | tick = List.foldl (<|) defaultTickConfig attributes }



-- View


{-| Configure the default view with style attributes.
-}
viewStatic : List (Attribute c) -> Attribute { b | view : View.ViewOption a c msg }
viewStatic styles config =
    { config | view = View.FromStyle styles }


{-| Configure the default view with style attributes based on the view value.
-}
viewDynamic : (a -> List (Attribute c)) -> Attribute { b | view : View.ViewOption a c msg }
viewDynamic toStyle config =
    { config | view = View.FromStyleDynamic toStyle }


{-| Use a custom view based on the view value.
-}
viewCustom : (a -> Svg.Svg msg) -> Attribute { b | view : View.ViewOption a c msg }
viewCustom view config =
    { config | view = View.FromCustom view }



-- Formatting


type FormatOption v
    = FormatFromList (List String)
    | FormatFromFunc (v -> String)


format : FormatOption v -> Attribute { a | format : FormatOption v }
format formatter config =
    { config | format = formatter }



-- Axis attributes


type AnchorOption
    = AnchorInner
    | AnchorOuter


{-| Anchor the ticks/labels on the inside of the plot. By default they are anchored on the outside.
-}
anchor : AnchorOption -> Attribute { a | anchor : AnchorOption }
anchor anchor config =
    { config | anchor = anchor }


type PositionOption
    = PositionLowest
    | PositionHighest
    | PositionAtZero


{-| Position this axis to the lowest value on the opposite axis. E.g. if
 this attribute is added on an y-axis, it will be positioned to the left.
-}
position : PositionOption -> Attribute { a | position : PositionOption }
position position config =
    { config | position = position }


{-| Remove tick and value where the axis crosses the opposite axis.
-}
clearIntersections : Attribute { a | clearIntersections : Bool }
clearIntersections config =
    { config | clearIntersections = True }


{-| Set a fixed max width (in pixels) on your bars.
-}
maxBarWidth : Int -> Attribute { a | maxWidth : MaxWidth }
maxBarWidth max config =
    { config | maxWidth = Fixed max }


type MaxWidth
    = Fixed Int
    | Percentage Int


{-| Set a relative max width (in percentage) your bars.
-}
maxBarWidthPer : Int -> Attribute { a | maxWidth : MaxWidth }
maxBarWidthPer max config =
    { config | maxWidth = Percentage max }


{-| By default your bars are stacked by x. If you want to stack them y, add this attribute.
-}
stackByY : Attribute { a | stackBy : Orientation }
stackByY config =
    { config | stackBy = Y }


type ValuesOption
    = ValuesFromList (List Value)
    | ValuesFromDelta Value
    | ValuesAuto


{-| Specify at what values will be added a tick.

    myXAxis : Plot.Element msg
    myXAxis =
        Plot.xAxis
            [ Axis.tickValues [ 0, 1, 2, 4, 8 ] ]

 **Note:** If you add another attribute altering the values like `tickDelta` _after_ this attribute,
 then this attribute will have no effect.
-}
values : ValuesOption -> Attribute { a | values : ValuesOption }
values values config =
    { config | values = values }
