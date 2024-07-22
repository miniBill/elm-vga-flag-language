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
import VFLParser exposing (Env, Item(..), Structure(..), Value(..))


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
    Html.div []
        [ Html.textarea
            [ Html.Attributes.value input
            , Html.Events.onInput identity
            , Html.Attributes.rows 10
            , Html.Attributes.cols 30
            ]
            []
        , case Parser.run VFLParser.mainParser input of
            Ok vfl ->
                Svg.svg
                    [ [ 0, 0, width, height ]
                        |> List.map String.fromInt
                        |> String.join " "
                        |> Svg.viewBox
                    , Html.Attributes.style "width" "100%"
                    , Html.Attributes.style "max-height" "90vh"
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

                (Command _) :: tail ->
                    Debug.todo "branch 'Command _ :: _' not implemented"
    in
    go VFLParser.defaultEnv items []


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
