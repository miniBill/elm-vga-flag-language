module Main exposing (main)

import Browser
import Color exposing (Color)
import Dict
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Parser
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
                Svg.svg
                    [ [ 0, 0, width, height ]
                        |> List.map String.fromInt
                        |> String.join " "
                        |> Svg.viewBox
                    , Html.Attributes.style "width" "100%"
                    , Html.Attributes.style "max-height" "70vh"
                    ]
                    (displayVFL vfl)

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


displayVFL : List VFLParser.Item -> List (Svg msg)
displayVFL items =
    let
        go env queue acc =
            case queue of
                [] ->
                    List.reverse acc

                (Variable name value) :: tail ->
                    let
                        evald =
                            eval env value
                    in
                    go (Dict.insert name evald env) tail acc

                (Structure (Vertical colors)) :: tail ->
                    let
                        count =
                            List.length colors

                        slice =
                            height // count

                        viewSlice i color =
                            Svg.rect
                                [ Svg.x "0"
                                , Svg.y <| String.fromInt (slice * i)
                                , Svg.width (String.fromInt width)
                                , Svg.height (String.fromInt slice)
                                , Svg.fill (Color.toCssString (asColor <| eval env color))
                                ]
                                []

                        group =
                            colors
                                |> List.indexedMap viewSlice
                                |> Svg.g []
                    in
                    go env tail (group :: acc)

                (Structure (Horizontal colors)) :: tail ->
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
                                , Svg.fill (Color.toCssString (asColor (eval env color)))
                                ]
                                []

                        group =
                            colors
                                |> List.indexedMap viewSlice
                                |> Svg.g []
                    in
                    go env tail (group :: acc)

                (Command (Rectangle ul br color)) :: tail ->
                    let
                        ( x, y ) =
                            asPoint (eval env ul)

                        ( ox, oy ) =
                            asPoint (eval env br)

                        w =
                            ox - x

                        h =
                            oy - y

                        c =
                            asColor <| eval env color

                        rect =
                            Svg.rect
                                [ Svg.x (String.fromInt x)
                                , Svg.y (String.fromInt y)
                                , Svg.width (String.fromInt w)
                                , Svg.height (String.fromInt h)
                                , Svg.fill (Color.toCssString c)
                                ]
                                []
                    in
                    go env tail (rect :: acc)

                (Command (Ellipse _ _ _ _)) :: _ ->
                    Debug.todo "branch 'Command (Ellipse _ _ _ _) :: _' not implemented"

                (Command (Polygon fillType base points color)) :: tail ->
                    case asFill (eval env fillType) of
                        Filled ->
                            let
                                c =
                                    asColor (eval env color)

                                basePoint =
                                    asPoint (eval env base)

                                poly : Svg msg
                                poly =
                                    Svg.polygon
                                        [ Svg.fill (Color.toCssString c)
                                        , polygonPoints env basePoint points
                                        ]
                                        []
                            in
                            go env tail (poly :: acc)

                        Outlined ->
                            let
                                c =
                                    asColor (eval env color)

                                basePoint =
                                    asPoint (eval env base)

                                poly : Svg msg
                                poly =
                                    Svg.polygon
                                        [ Svg.strokeWidth "1"
                                        , Svg.stroke (Color.toCssString c)
                                        , Svg.fill "transparent"
                                        , polygonPoints env basePoint points
                                        ]
                                        []
                            in
                            go env tail (poly :: acc)

                        ThickOutlines ->
                            let
                                c =
                                    asColor (eval env color)

                                basePoint =
                                    asPoint (eval env base)

                                poly : Svg msg
                                poly =
                                    Svg.polygon
                                        [ Svg.strokeWidth "3"
                                        , Svg.stroke (Color.toCssString c)
                                        , Svg.fill "transparent"
                                        , polygonPoints env basePoint points
                                        ]
                                        []
                            in
                            go env tail (poly :: acc)

                (Command (Lines _ _ _ _)) :: _ ->
                    -- Thick is 3 pixels
                    Debug.todo "branch 'Command (Lines _ _ _ _) :: _' not implemented"

                (Command (Image _)) :: _ ->
                    Debug.todo "branch 'Command (Image _) :: _' not implemented"
    in
    go VFLParser.defaultEnv items []


polygonPoints : Env -> Point -> Expr -> Svg.Attribute msg
polygonPoints env ( bx, by ) points =
    points
        |> eval env
        |> asListOf asPoint
        |> List.map
            (\( px, py ) ->
                pointToString ( bx + px, by + py )
            )
        |> String.join " "
        |> Svg.points


asListOf : (Value -> v) -> Value -> List v
asListOf itemMap value =
    value
        |> asList
        |> List.map itemMap


asPoint : Value -> Point
asPoint value =
    case value of
        Point p ->
            p

        _ ->
            Debug.todo ("Expected Point, got " ++ Debug.toString value)


asList : Value -> List Value
asList value =
    case value of
        List l ->
            l

        _ ->
            Debug.todo ("Expected List, got " ++ Debug.toString value)


asFill : Value -> Fill
asFill value =
    case value of
        Fill f ->
            f

        _ ->
            Debug.todo ("Expected Fill, got " ++ Debug.toString value)


asColor : Value -> Color
asColor value =
    case value of
        Color c ->
            c

        _ ->
            Debug.todo ("Expected Color, got " ++ Debug.toString value)


pointToString : Point -> String
pointToString ( x, y ) =
    String.fromInt x ++ "," ++ String.fromInt y


eval : Env -> VFLParser.Expr -> VFLParser.Value
eval env val =
    case val of
        EList children ->
            List (List.map (eval env) children)

        EVariable n ->
            case Dict.get n env of
                Just v ->
                    v

                Nothing ->
                    Debug.todo <| "Undefined variable: " ++ n

        EPoint l r ->
            case eval env l of
                Int li ->
                    case eval env r of
                        Int ri ->
                            Point ( li, ri )

                        rv ->
                            Debug.todo (Debug.toString r ++ " (" ++ Debug.toString rv ++ ") is not an integer")

                lv ->
                    Debug.todo (Debug.toString l ++ " (" ++ Debug.toString lv ++ ") is not an integer")

        EValue value ->
            value
