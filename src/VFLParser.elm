module VFLParser exposing
    ( Command(..)
    , Env
    , Expr(..)
    , Fill(..)
    , Item(..)
    , LineType(..)
    , Point
    , Structure(..)
    , Value(..)
    , defaultEnv
    , mainParser
    )

import Color exposing (Color)
import Dict exposing (Dict)
import Hex
import Parser exposing ((|.), (|=), Parser)
import Parser.Extras
import Parser.Workaround


type Item
    = Variable String Expr
    | Structure Structure
    | Command Command


type Command
    = Rectangle Expr Expr Expr
    | Ellipse Expr Expr Expr Expr
    | Polygon Expr Expr Expr Expr
    | Lines Expr Expr Expr Expr
    | Image Expr


type Structure
    = Vertical (List Expr)
    | Horizontal (List Expr)


type Value
    = Int Int
    | Color Color
    | Fill Fill
    | LineType LineType
    | String String
    | Point Point
    | List (List Value)


type Expr
    = EValue Value
    | EVariable String
    | EPoint Expr Expr
    | EList (List Expr)


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
            |= exprParser
            |. symbol ","
            |= exprParser
            |. symbol ","
            |= exprParser
        , Parser.succeed Ellipse
            |. symbol "Ellipse"
            |= exprParser
            |. symbol ","
            |= exprParser
            |. symbol ","
            |= exprParser
            |. symbol ","
            |= exprParser
        , Parser.succeed Polygon
            |. symbol "Polygon"
            |= exprParser
            |. symbol ","
            |= exprParser
            |. symbol ","
            |= exprParser
            |. symbol ","
            |= exprParser
        , Parser.succeed Lines
            |. symbol "Lines"
            |= exprParser
            |. symbol ","
            |= exprParser
            |. symbol ","
            |= exprParser
            |. symbol ","
            |= exprParser
        , Parser.succeed Image
            |. symbol "Image"
            |= exprParser
        ]


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
            , item = exprParser
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
hexFromString hx =
    case Hex.fromString (String.toLower hx) of
        Ok i ->
            Parser.succeed i

        Err e ->
            Parser.problem e


variableParser : Parser Item
variableParser =
    Parser.succeed Variable
        |. Parser.symbol "$"
        |= Parser.getChompedString (Parser.chompWhile isNameChar)
        |. spaces
        |. symbol "="
        |= exprParser


isNameChar : Char -> Bool
isNameChar c =
    (c /= '=')
        && (c /= ' ')
        && (c /= '\n')
        && (c /= '\u{000D}')
        && (c /= '\t')


exprParser : Parser Expr
exprParser =
    Parser.oneOf
        [ Parser.map (EValue << Color) colorParser
        , Parser.map (EValue << Int) intParser
        , Parser.map (EValue << String) stringParser
        , Parser.succeed EList
            |= listParser
        , pointParser
        , Parser.succeed EVariable
            |. Parser.symbol "$"
            |= Parser.getChompedString (Parser.chompWhile isNameChar)
            |. spaces
        , Parser.map (EValue << Fill) fillParser
        , Parser.map (EValue << LineType) lineTypeParser
        ]


listParser : Parser (List Expr)
listParser =
    Parser.sequence
        { start = "["
        , spaces = spaces
        , separator = ","
        , item = Parser.lazy (\_ -> exprParser)
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


pointParser : Parser Expr
pointParser =
    Parser.succeed EPoint
        |. symbol "("
        |= Parser.lazy (\_ -> exprParser)
        |. symbol ","
        |= Parser.lazy (\_ -> exprParser)
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
