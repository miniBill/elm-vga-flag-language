module Main exposing (main)

import Browser
import Color exposing (Color)
import Dict
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Parser
import Result.Extra
import Svg exposing (Svg)
import Svg.Attributes as Svg
import VFLParser exposing (Command(..), Env, Expr(..), Fill(..), Item(..), Point, Structure(..), Value(..))


type alias Model =
    String


type alias Msg =
    String


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , view = view
        , update = always
        }


init : Model
init =
    """$names=["demisexual"]
$description="The Demisexual pride flag"
// Unknown creator. Maybe research further?
$textcolor=$black
// The trick here is that we define the top&bottom colors twice, so this ends up looking like we have variable heights


$center_stripe = RGB(110,  0,112)
$bottom = RGB(210,210,210)
vertical{
\t$white 
\t$white
\t$center_stripe
\t$bottom
\t$bottom
}

Polygon Filled, (0,0), [(0,0), (240,240), (0,480)], $black"""


width : number
width =
    640


height : number
height =
    480


view : Model -> Html Msg
view input =
    Html.div
        [ Html.Attributes.style "padding" "8px"
        , Html.Attributes.style "gap" "8px"
        , Html.Attributes.style "display" "flex"
        , Html.Attributes.style "flex-direction" "column"
        ]
        [ Html.textarea
            [ Html.Attributes.value input
            , Html.Events.onInput identity
            , Html.Attributes.rows 10
            , Html.Attributes.cols 30
            ]
            []
        , case Parser.run VFLParser.mainParser input of
            Ok _ ->
                Html.text ""

            Err _ ->
                input
                    |> String.split "\n"
                    |> List.indexedMap
                        (\i l ->
                            String.padLeft 3 ' ' (String.fromInt (i + 1))
                                ++ " "
                                ++ l
                        )
                    |> String.join "\n"
                    |> Html.text
                    |> List.singleton
                    |> Html.pre []
        , case Parser.run VFLParser.mainParser input of
            Ok vfl ->
                case displayVFL vfl of
                    Err e ->
                        Html.text e

                    Ok svg ->
                        Svg.svg
                            [ [ 0, 0, width, height ]
                                |> List.map String.fromInt
                                |> String.join " "
                                |> Svg.viewBox
                            , Html.Attributes.style "width" "100%"
                            , Html.Attributes.style "max-height" "70vh"
                            ]
                            svg

            Err errs ->
                errs
                    |> List.map
                        (\err ->
                            Html.div []
                                [ Html.text (viewError err)
                                ]
                        )
                    |> Html.div []
        ]


viewError : Parser.DeadEnd -> String
viewError { row, col, problem } =
    String.fromInt row
        ++ ":"
        ++ String.fromInt col
        ++ " "
        ++ Debug.toString problem


displayVFL : List VFLParser.Item -> Result String (List (Svg msg))
displayVFL items =
    items
        |> resultFoldl displayStep ( VFLParser.defaultEnv, [] )
        |> Result.map (\( _, acc ) -> List.reverse acc)


resultFoldl :
    (a -> acc -> Result err acc)
    -> acc
    -> List a
    -> Result err acc
resultFoldl f acc list =
    case list of
        [] ->
            Ok acc

        head :: tail ->
            case f head acc of
                Err e ->
                    Err e

                Ok v ->
                    resultFoldl f v tail


displayStep :
    Item
    -> ( Env, List (Svg msg) )
    -> Result String ( Env, List (Svg msg) )
displayStep elem ( env, acc ) =
    let
        prepend :
            Result error (Svg msg)
            -> Result error ( Env, List (Svg msg) )
        prepend res =
            Result.map (\g -> ( env, g :: acc )) res
    in
    case elem of
        Variable name value ->
            eval env value
                |> Result.map
                    (\evald ->
                        ( Dict.insert name evald env
                        , acc
                        )
                    )

        Structure (Vertical colors) ->
            colors
                |> Result.Extra.combineMap (evalAs asColor env)
                |> Result.map
                    (\colorList ->
                        let
                            count : Int
                            count =
                                List.length colors

                            slice : Int
                            slice =
                                height // count

                            viewSlice : Int -> Color -> Svg msg
                            viewSlice i color =
                                Svg.rect
                                    [ Svg.x "0"
                                    , Svg.y <| String.fromInt (slice * i)
                                    , Svg.width (String.fromInt width)
                                    , Svg.height (String.fromInt slice)
                                    , Svg.fill (Color.toCssString color)
                                    ]
                                    []
                        in
                        colorList
                            |> List.indexedMap viewSlice
                            |> Svg.g []
                    )
                |> prepend

        Structure (Horizontal colors) ->
            colors
                |> Result.Extra.combineMap (evalAs asColor env)
                |> Result.map
                    (\colorList ->
                        let
                            count =
                                List.length colors

                            slice =
                                width // count

                            viewSlice i color =
                                Svg.rect
                                    [ Svg.x <| String.fromInt (slice * i)
                                    , Svg.y "0"
                                    , Svg.width (String.fromInt slice)
                                    , Svg.height (String.fromInt height)
                                    , Svg.fill (Color.toCssString color)
                                    ]
                                    []
                        in
                        colorList
                            |> List.indexedMap viewSlice
                            |> Svg.g []
                    )
                |> prepend

        Command (Rectangle ul br color) ->
            Result.map3 viewRectangle
                (evalAs asPoint env ul)
                (evalAs asPoint env br)
                (evalAs asColor env color)
                |> prepend

        Command (Ellipse _ _ _ _) ->
            Debug.todo "branch 'Command (Ellipse _ _ _ _) :: _' not implemented"

        Command (Polygon fill base points color) ->
            Result.map4 viewPolygon
                (evalAs asFill env fill)
                (evalAs asPoint env base)
                (evalAs (asListOf asPoint) env points)
                (evalAs asColor env color)
                |> prepend

        Command (Lines _ _ _ _) ->
            -- Thick is 3 pixels
            Debug.todo "branch 'Command (Lines _ _ _ _) :: _' not implemented"

        Command (Image _) ->
            Debug.todo "branch 'Command (Image _) :: _' not implemented"


viewRectangle : ( Int, Int ) -> ( Int, Int ) -> Color -> Svg msg
viewRectangle ( x, y ) ( ox, oy ) color =
    let
        w =
            ox - x

        h =
            oy - y
    in
    Svg.rect
        [ Svg.x (String.fromInt x)
        , Svg.y (String.fromInt y)
        , Svg.width (String.fromInt w)
        , Svg.height (String.fromInt h)
        , Svg.fill (Color.toCssString color)
        ]
        []


viewPolygon : Fill -> Point -> List Point -> Color -> Svg msg
viewPolygon fill base points color =
    case fill of
        Filled ->
            Svg.polygon
                [ Svg.fill (Color.toCssString color)
                , polygonPoints base points
                ]
                []

        Outlined ->
            Svg.polygon
                [ Svg.strokeWidth "1"
                , Svg.stroke (Color.toCssString color)
                , Svg.fill "transparent"
                , polygonPoints base points
                ]
                []

        ThickOutlines ->
            Svg.polygon
                [ Svg.strokeWidth "3"
                , Svg.stroke (Color.toCssString color)
                , Svg.fill "transparent"
                , polygonPoints base points
                ]
                []


polygonPoints : Point -> List Point -> Svg.Attribute msg
polygonPoints ( bx, by ) points =
    points
        |> List.map
            (\( px, py ) ->
                pointToString ( bx + px, by + py )
            )
        |> String.join " "
        |> Svg.points


asListOf : (Value -> Result String v) -> Value -> Result String (List v)
asListOf itemMap value =
    value
        |> asList
        |> Result.andThen (Result.Extra.combineMap itemMap)


asPoint : Value -> Result String Point
asPoint value =
    case value of
        Point p ->
            Ok p

        _ ->
            Err ("Expected Point, got " ++ valueToString value)


asInt : Value -> Result String Int
asInt value =
    case value of
        Int i ->
            Ok i

        _ ->
            Err ("Expected Int, got " ++ valueToString value)


valueToString : Value -> String
valueToString value =
    Debug.toString value


asList : Value -> Result String (List Value)
asList value =
    case value of
        List l ->
            Ok l

        _ ->
            Err ("Expected List, got " ++ valueToString value)


asFill : Value -> Result String Fill
asFill value =
    case value of
        Fill f ->
            Ok f

        _ ->
            Err ("Expected Fill, got " ++ valueToString value)


asColor : Value -> Result String Color
asColor value =
    case value of
        Color c ->
            Ok c

        _ ->
            Err ("Expected Color, got " ++ valueToString value)


pointToString : Point -> String
pointToString ( x, y ) =
    String.fromInt x ++ "," ++ String.fromInt y


evalAs :
    (Value -> Result String v)
    -> Env
    -> Expr
    -> Result String v
evalAs converter env val =
    eval env val
        |> Result.andThen converter


eval : Env -> VFLParser.Expr -> Result String VFLParser.Value
eval env val =
    case val of
        EList children ->
            children
                |> Result.Extra.combineMap (eval env)
                |> Result.map List

        EVariable n ->
            case Dict.get n env of
                Just v ->
                    Ok v

                Nothing ->
                    Err <| "Undefined variable: " ++ n

        EPoint l r ->
            Result.map2
                (\li ri ->
                    Point ( li, ri )
                )
                (Result.andThen asInt (eval env l))
                (Result.andThen asInt (eval env r))

        EValue value ->
            Ok value
