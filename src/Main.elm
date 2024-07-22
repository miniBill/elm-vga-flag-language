module Main exposing (main)

import Browser
import Color
import Dict
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Parser
import Svg exposing (Svg)
import Svg.Attributes as Svg
import VFLParser exposing (Command(..), Env, Fill(..), Item(..), Point, Structure(..), Value(..))


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

vertical{
\tRGB(255,255,255)
\tRGB(255,255,255)
\tRGB(110,  0,112)
\tRGB(210,210,210)
\tRGB(210,210,210)
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
        [ Html.Attributes.style "padding" "10px" ]
        [ Html.textarea
            [ Html.Attributes.value input
            , Html.Events.onInput identity
            , Html.Attributes.rows 10
            , Html.Attributes.cols 30
            ]
            []
        , input
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
                    , Html.Attributes.style "width" "90%"
                    , Html.Attributes.style "padding-left" "5%"
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
                                , Svg.fill (Color.toCssString color)
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
                                , Svg.fill (Color.toCssString color)
                                ]
                                []

                        group =
                            colors
                                |> List.indexedMap viewSlice
                                |> Svg.g []
                    in
                    go env tail (group :: acc)

                (Command (Rectangle _ _ _)) :: tail ->
                    Debug.todo "branch 'Command (Rectangle _ _ _) :: _' not implemented"

                (Command (Ellipse _ _ _ _)) :: _ ->
                    Debug.todo "branch 'Command (Ellipse _ _ _ _) :: _' not implemented"

                (Command (Polygon Filled start points color)) :: tail ->
                    case eval env color of
                        Color c ->
                            let
                                poly : Svg msg
                                poly =
                                    Svg.polygon
                                        [ Svg.fill (Color.toCssString c)
                                        , (start :: points)
                                            |> List.map pointToString
                                            |> String.join " "
                                            |> Svg.points
                                        ]
                                        []
                            in
                            go env tail (poly :: acc)

                        _ ->
                            Debug.todo (Debug.toString color ++ " is not a color ")

                (Command (Polygon Outlined _ _ _)) :: _ ->
                    Debug.todo "branch 'Command (Polygon Outlined _ _ _) :: _' not implemented"

                (Command (Polygon ThickOutlines _ _ _)) :: _ ->
                    Debug.todo "branch 'Command (Polygon ThickOutlines _ _ _) :: _' not implemented"

                (Command (Lines _ _ _ _)) :: _ ->
                    Debug.todo "branch 'Command (Lines _ _ _ _) :: _' not implemented"

                (Command (Image _)) :: _ ->
                    Debug.todo "branch 'Command (Image _) :: _' not implemented"
    in
    go VFLParser.defaultEnv items []


pointToString : Point -> String
pointToString ( x, y ) =
    String.fromInt x ++ "," ++ String.fromInt y


eval : Env -> VFLParser.Value -> VFLParser.Value
eval env val =
    case val of
        List children ->
            List (List.map (eval env) children)

        Name n ->
            case Dict.get n env of
                Just v ->
                    v

                Nothing ->
                    Debug.todo "branch 'Name _' not implemented"

        _ ->
            val
