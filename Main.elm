module Main exposing (..)

import Html exposing (beginnerProgram)
import Svg
import Svg exposing (..)
import Svg.Attributes exposing (..)


type Command
    = Forward Float
    | Turn Float


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
left 90
forward 100
    """


parseProgram : String -> List Command
parseProgram input =
    String.split "\n" input
        |> List.filterMap
            (\line ->
                case String.split " " line of
                    [ "forward", distance ] ->
                        case String.toFloat distance of
                            Ok d ->
                                Just (Forward d)

                            _ ->
                                Nothing

                    [ "right", angle ] ->
                        case String.toFloat angle of
                            Ok a ->
                                Just (Turn a)

                            _ ->
                                Nothing

                    [ "left", angle ] ->
                        case String.toFloat angle of
                            Ok a ->
                                Just (Turn -a)

                            _ ->
                                Nothing

                    _ ->
                        Nothing
            )


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
            [ d (commandsToPath model.program)
            , stroke "black"
            , strokeWidth "3px"
            ]
            []
        ]


commandsToPath : List Command -> String
commandsToPath commands =
    "M0 0 l10 20"


main : Program Never Model Msg
main =
    beginnerProgram
        { model = init
        , update = update
        , view = view
        }
