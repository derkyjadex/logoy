module Main exposing (..)

import Html exposing (beginnerProgram)
import Svg
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Debug


type Command
    = Forward Float
    | Turn Float
    | Repeat Int (List Command)


type alias Model =
    { program : List Command
    }


type alias Msg =
    {}


programText : String
programText =
    """
forward 100
right 90
forward 50
right 135
forward 200
repeat 5 {
    right 144
    forward 20
}
left 90
forward 100
    """


type ParseState
    = Good { input : List String, output : List Command }
    | Error String


parseProgram : String -> List Command
parseProgram input =
    let
        result =
            Debug.log "" <| parseProgram2 (Good { input = String.split "\n" input, output = [] })
    in
        case result of
            Good state ->
                state.output

            Error _ ->
                []


parseProgram2 : ParseState -> ParseState
parseProgram2 s =
    case Debug.log ">" s of
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
                            parseProgram2 <| Debug.log "skipping" <| Good { state | input = rest }

                        [ "forward", distance ] ->
                            case String.toFloat distance of
                                Ok d ->
                                    parseProgram2 <| Good { input = rest, output = state.output ++ [ Forward d ] }

                                _ ->
                                    Error "distance not a float"

                        [ "right", angle ] ->
                            case String.toFloat angle of
                                Ok a ->
                                    parseProgram2 <| Good { input = rest, output = state.output ++ [ Turn a ] }

                                _ ->
                                    Error "angle not a float"

                        [ "left", angle ] ->
                            case String.toFloat angle of
                                Ok a ->
                                    parseProgram2 <| Good { input = rest, output = state.output ++ [ Turn -a ] }

                                _ ->
                                    Error "angle not a float"

                        [ "repeat", number, "{" ] ->
                            case String.toInt number of
                                Ok n ->
                                    let
                                        subState =
                                            parseProgram2 <| Good { input = rest, output = [] }
                                    in
                                        case subState of
                                            Good t ->
                                                parseProgram2 <|
                                                    Good
                                                        { input = t.input
                                                        , output = state.output ++ [ Repeat n t.output ]
                                                        }

                                            Error e ->
                                                Error e

                                _ ->
                                    Error "repeat number not an integer"

                        _ ->
                            Error ("unknown command: " ++ line)


init : Model
init =
    { program = parseProgram programText }


update : Msg -> Model -> Model
update msg model =
    model


view : Model -> Svg Msg
view model =
    svg
        [ width "500", height "300", viewBox "-250 -150 500 300" ]
        [ rect [ x "-250", y "-150", width "500", height "300", fill "rgb(90%, 90%, 90%)" ] []
        , Svg.path
            [ d ("M0 0 " ++ commandsToPath model.program)
            , stroke "black"
            , strokeWidth "3px"
            , fill "none"
            ]
            []
        ]


type alias DrawState =
    { output : String, angle : Float }


commandsToPath : List Command -> String
commandsToPath commands =
    let
        result =
            commandsToPath2 commands -90
    in
        result.output


commandsToPath2 : List Command -> Float -> DrawState
commandsToPath2 commands angle =
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
                            commandsToPath2 (List.concat <| List.repeat n cmds) state.angle
                    in
                        { output = state.output ++ result.output ++ " "
                        , angle = result.angle
                        }
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
