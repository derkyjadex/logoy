module Main exposing (..)

import Html exposing (beginnerProgram)
import Dict
import Dict exposing (Dict)
import Svg
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Html exposing (div, textarea)
import Html.Attributes as Html
import Html.Events exposing (onInput)
import Debug


type Command
    = Forward Float
    | Turn Float
    | Repeat Int (List Command)
    | Call String


type alias Model =
    { text : String
    , error : String
    , program : List Command
    , subs : Dict String (List Command)
    }


type Msg
    = UpdateText String


programText : String
programText =
    """
right 90
forward 90
right 90
forward 25

define star {
    repeat 5 {
        right 144
        forward 30
    }
}

repeat 12 {
    right 30
    forward 50
    call star

    """


type ParseState
    = Good
        { input : List String
        , output : List Command
        , subs : Dict String (List Command)
        }
    | Error String


parseProgram : Model -> Model
parseProgram model =
    let
        result =
            parseProgram2
                (Good
                    { input = String.split "\n" model.text
                    , output = []
                    , subs = Dict.empty
                    }
                )
    in
        case Debug.log "result" result of
            Good state ->
                { model
                    | error = ""
                    , program = state.output
                    , subs = state.subs
                }

            Error error ->
                { model
                    | error = error
                    , program = []
                    , subs = Dict.empty
                }


parseProgram2 : ParseState -> ParseState
parseProgram2 s =
    case s of
        Error _ ->
            s

        Good state ->
            case state.input of
                [] ->
                    Good state

                line :: rest ->
                    case String.split " " (String.trim line) of
                        [ "}" ] ->
                            Good { state | input = rest }

                        [ "" ] ->
                            parseProgram2 <| Good { state | input = rest }

                        [ "forward", distance ] ->
                            case String.toFloat distance of
                                Ok d ->
                                    parseProgram2 <|
                                        Good
                                            { state
                                                | input = rest
                                                , output = state.output ++ [ Forward d ]
                                            }

                                _ ->
                                    Error "distance not a float"

                        [ "right", angle ] ->
                            case String.toFloat angle of
                                Ok a ->
                                    parseProgram2 <|
                                        Good
                                            { state
                                                | input = rest
                                                , output = state.output ++ [ Turn a ]
                                            }

                                _ ->
                                    Error "angle not a float"

                        [ "left", angle ] ->
                            case String.toFloat angle of
                                Ok a ->
                                    parseProgram2 <|
                                        Good
                                            { state
                                                | input = rest
                                                , output = state.output ++ [ Turn -a ]
                                            }

                                _ ->
                                    Error "angle not a float"

                        [ "repeat", number, "{" ] ->
                            case String.toInt number of
                                Ok n ->
                                    let
                                        subState =
                                            parseProgram2 <|
                                                Good
                                                    { input = rest
                                                    , output = []
                                                    , subs = state.subs
                                                    }
                                    in
                                        case subState of
                                            Good t ->
                                                parseProgram2 <|
                                                    Good
                                                        { input = t.input
                                                        , output = state.output ++ [ Repeat n t.output ]
                                                        , subs = t.subs
                                                        }

                                            Error e ->
                                                Error e

                                _ ->
                                    Error "repeat number not an integer"

                        [ "define", name, "{" ] ->
                            let
                                subState =
                                    parseProgram2 <|
                                        Good
                                            { input = rest
                                            , output = []
                                            , subs = state.subs
                                            }
                            in
                                case subState of
                                    Good t ->
                                        parseProgram2 <|
                                            Good
                                                { input = t.input
                                                , output = state.output
                                                , subs = Dict.insert name t.output t.subs
                                                }

                                    Error e ->
                                        Error e

                        [ "call", name ] ->
                            case Dict.get name state.subs of
                                Just _ ->
                                    parseProgram2 <|
                                        Good
                                            { state
                                                | input = rest
                                                , output = state.output ++ [ Call name ]
                                            }

                                Nothing ->
                                    Error ("unknown sub: " ++ name)

                        _ ->
                            Error ("unknown command: " ++ line)


init : Model
init =
    parseProgram
        { text = programText
        , error = ""
        , program = []
        , subs = Dict.empty
        }


update : Msg -> Model -> Model
update msg model =
    case msg of
        UpdateText text ->
            parseProgram { model | text = text }


view : Model -> Svg Msg
view model =
    div []
        [ svg
            [ width "500", height "300", viewBox "-250 -150 500 300" ]
            [ rect [ x "-250", y "-150", width "500", height "300", fill "rgb(90%, 90%, 90%)" ] []
            , Svg.path
                [ d ("M0 0 " ++ commandsToPath model.program model.subs)
                , stroke "black"
                , strokeWidth "3px"
                , fill "none"
                ]
                []
            ]
        , div []
            [ textarea
                [ Html.style
                    [ ( "width", "490px" )
                    , ( "height", "290px" )
                    , ( "padding", "2px" )
                    , ( "border", "1px solid black" )
                    , ( "font-family", "monospace" )
                    ]
                , onInput (UpdateText)
                ]
                [ text model.text ]
            ]
        , div [] [ text model.error ]
        ]


type alias DrawState =
    { output : String, angle : Float }


commandsToPath : List Command -> Dict String (List Command) -> String
commandsToPath commands subs =
    let
        result =
            commandsToPath2 commands subs -90
    in
        result.output


commandsToPath2 : List Command -> Dict String (List Command) -> Float -> DrawState
commandsToPath2 commands subs angle =
    List.foldl
        (\c state ->
            case c of
                Forward d ->
                    { state | output = state.output ++ "l" ++ getVector state.angle d ++ " " }

                Turn a ->
                    { state | angle = state.angle + a }

                Repeat n cmds ->
                    let
                        result =
                            commandsToPath2 (List.concat <| List.repeat n cmds) subs state.angle
                    in
                        { output = state.output ++ result.output
                        , angle = result.angle
                        }

                Call n ->
                    case Dict.get n subs of
                        Just cmds ->
                            let
                                result =
                                    commandsToPath2 cmds subs state.angle
                            in
                                { output = state.output ++ result.output
                                , angle = result.angle
                                }

                        Nothing ->
                            state
        )
        { output = "", angle = angle }
        commands


getVector : Float -> Float -> String
getVector angle distance =
    let
        x =
            distance * cos (pi * angle / 180)

        y =
            distance * sin (pi * angle / 180)
    in
        (toString x) ++ " " ++ (toString y)


main : Program Never Model Msg
main =
    beginnerProgram
        { model = init
        , update = update
        , view = view
        }
