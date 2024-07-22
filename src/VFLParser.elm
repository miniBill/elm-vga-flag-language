module VFLParser exposing
    ( Command(..)
    , Env
    , Fill(..)
    , Item(..)
    , LineType(..)
    , Point
    , Structure(..)
    , Value(..)
    , defaultEnv
    , item
    , mainParser
    , spaces
    )

import Color exposing (Color)
import Dict exposing (Dict)
import Parser exposing ((|.), (|=), Parser)
import Parser.Extras
import Parser.Workaround


type Item
    = Variable String Value
    | Structure Structure
    | Command Command


type Command
    = Rectangle Point Point Value
    | Ellipse Point Int Int Value
    | Polygon Fill Point (List Point) Value
    | Lines LineType Point (List Point) Value
    | Image String


type Structure
    = Vertical (List Color)
    | Horizontal (List Color)


type Value
    = Int Int
    | Color Color
    | Fill Fill
    | LineType LineType
    | String String
    | Point Point
    | List (List Value)
    | Name String


type Fill
    = Filled
    | Outlined
    | ThickOutlines


type LineType
    = Regular
    | Thicc


type alias Point =
    ( Int, Int )


type alias Env =
    Dict String Value


defaultEnv : Env
defaultEnv =
    [ ( "black", Color Color.black )
    , ( "white", Color Color.white )
    ]
        |> Dict.fromList


mainParser : Parser.Parser (List Item)
mainParser =
    Parser.sequence
        { start = ""
        , separator = ""
        , end = ""
        , item = item
        , spaces = spaces
        , trailing = Parser.Optional
        }
        |. Parser.end


item : Parser.Parser Item
item =
    Parser.oneOf
        [ variableParser
        , Parser.map Structure structureParser
        , Parser.map Command commandParser
        ]


commandParser : Parser Command
commandParser =
    Parser.oneOf
        [ Parser.succeed Rectangle
            |. symbol "Rectangle"
            |= pointParser
            |. symbol ","
            |= pointParser
            |. symbol ","
            |= valueParser
        , Parser.succeed Ellipse
            |. symbol "Ellipse"
            |= pointParser
            |. symbol ","
            |= intParser
            |. symbol ","
            |= intParser
            |. symbol ","
            |= valueParser
        , Parser.succeed Polygon
            |. symbol "Polygon"
            |= fillParser
            |. symbol ","
            |= pointParser
            |. symbol ","
            |= pointListParser
            |. symbol ","
            |= valueParser
        , Parser.succeed Lines
            |. symbol "Lines"
            |= lineTypeParser
            |. symbol ","
            |= pointParser
            |. symbol ","
            |= pointListParser
            |. symbol ","
            |= valueParser
        , Parser.succeed Image
            |. symbol "Image"
            |= stringParser
        ]


pointListParser : Parser (List Point)
pointListParser =
    Parser.sequence
        { start = "["
        , spaces = spaces
        , separator = ","
        , item = pointParser
        , end = "]"
        , trailing = Parser.Forbidden
        }
        |. spaces


fillParser : Parser Fill
fillParser =
    Parser.oneOf
        [ Parser.succeed Filled
            |. symbol "Filled"
        , Parser.succeed Outlined
            |. symbol "Outlined"
        , Parser.succeed ThickOutlines
            |. symbol "ThickOutlines"
        ]


lineTypeParser : Parser LineType
lineTypeParser =
    Parser.oneOf
        [ Parser.succeed Regular
            |. symbol "Regular"
        , Parser.succeed Thicc
            |. symbol "Thick"
        ]


symbol : String -> Parser ()
symbol s =
    Parser.symbol s
        |. spaces


structureParser : Parser Structure
structureParser =
    Parser.succeed identity
        |= Parser.oneOf
            [ Parser.succeed Vertical
                |. symbol "vertical"
            , Parser.succeed Horizontal
                |. symbol "horizontal"
            ]
        |= Parser.sequence
            { start = "{"
            , end = "}"
            , separator = ""
            , spaces = spaces
            , trailing = Parser.Optional
            , item = colorParser
            }
        |. spaces


colorParser : Parser Color
colorParser =
    Parser.oneOf
        [ Parser.succeed Color.rgb255
            |. symbol "RGB"
            |. symbol "("
            |= intParser
            |. symbol ","
            |= intParser
            |. symbol ","
            |= intParser
            |. symbol ")"
        , Parser.succeed Color.rgb255
            |. symbol "#"
            |= hexPair
            |= hexPair
            |= hexPair
        ]


hexPair : Parser Int
hexPair =
    (Parser.chompIf Char.isHexDigit
        |. Parser.chompIf Char.isHexDigit
        |> Parser.getChompedString
        |> Parser.andThen hexFromString
    )
        |. spaces


hexFromString : String -> Parser Int
hexFromString _ =
    Debug.todo "hexFromString"


variableParser : Parser Item
variableParser =
    Parser.succeed Variable
        |. Parser.symbol "$"
        |= Parser.getChompedString (Parser.chompWhile isNameChar)
        |. spaces
        |. symbol "="
        |= valueParser


isNameChar : Char -> Bool
isNameChar c =
    (c /= '=')
        && (c /= ' ')
        && (c /= '\n')
        && (c /= '\u{000D}')
        && (c /= '\t')


valueParser : Parser Value
valueParser =
    Parser.oneOf
        [ Parser.map Color colorParser
        , Parser.map Int intParser
        , Parser.map String stringParser
        , Parser.succeed List
            |= listParser
        , Parser.map Point pointParser
        , Parser.succeed Name
            |. Parser.symbol "$"
            |= Parser.getChompedString (Parser.chompWhile isNameChar)
            |. spaces
        ]


listParser : Parser (List Value)
listParser =
    Parser.sequence
        { start = "["
        , spaces = spaces
        , separator = ","
        , item = Parser.lazy (\_ -> valueParser)
        , end = "]"
        , trailing = Parser.Forbidden
        }
        |. spaces


stringParser : Parser String
stringParser =
    Parser.oneOf
        [ Parser.Extras.quotedString '\\' '"'
        , Parser.Extras.quotedString '\\' '\''
        ]
        |. spaces


pointParser : Parser Point
pointParser =
    Parser.succeed Tuple.pair
        |. symbol "("
        |= intParser
        |. symbol ","
        |= intParser
        |. symbol ")"


intParser : Parser Int
intParser =
    Parser.int
        |. spaces


spaces : Parser.Parser ()
spaces =
    let
        spc : Parser ()
        spc =
            Parser.chompWhile
                (\c ->
                    (c == ' ')
                        || (c == '\n')
                        || (c == '\u{000D}')
                        || (c == '\t')
                )
    in
    spc
        |. Parser.oneOf
            [ Parser.Workaround.lineCommentAfter "//"
                |. Parser.lazy (\_ -> spaces)
            , Parser.succeed ()
            ]
