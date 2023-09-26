module Main exposing (main)

import Html exposing (Html)


testInput : String
testInput =
    """
A Y
B X
C Z
"""


type alias Scores =
    { player : Int, opponent : Int }


init : Scores
init =
    { player = 0, opponent = 0 }


type Move
    = Rock
    | Paper
    | Scissors


moveValue : Maybe Move -> Int
moveValue x =
    case x of
        Just Rock ->
            1

        Just Paper ->
            2

        Just Scissors ->
            3

        Nothing ->
            0


moveCodeOpponent : String -> Maybe Move
moveCodeOpponent moveStr =
    case moveStr of
        "A" ->
            Just Rock

        "B" ->
            Just Paper

        "C" ->
            Just Scissors

        _ ->
            Nothing


moveCodePlayer : String -> Maybe Move
moveCodePlayer moveStr =
    case moveStr of
        "X" ->
            Just Rock

        "Y" ->
            Just Paper

        "Z" ->
            Just Scissors

        _ ->
            Nothing


scoresToString : Scores -> String
scoresToString scores =
    "Scores { player = " ++ String.fromInt scores.player ++ ", opponent = " ++ String.fromInt scores.opponent ++ " }"


main : Html msg
main =
    testInput
        |> String.trim
        |> String.lines
        |> partitionRounds
        |> determineRounds
        |> Debug.toString
        |> Html.text


partitionRounds : List String -> List (List String)
partitionRounds listOfRounds =
    List.map (\x -> String.split " " x) listOfRounds


determineRounds : List (List String) -> Scores
determineRounds rounds =
    List.foldl extractAndDetermineRound init rounds


extractAndDetermineRound : List String -> Scores -> Scores
extractAndDetermineRound pair scores =
    case pair of
        [ first, second ] ->
            determineRound first second scores

        _ ->
            scores


determineRound : String -> String -> Scores -> Scores
determineRound opponentMove playerMove scores =
    let
        opponentWon : Maybe Move -> Maybe Move -> ( Bool, Int, Int )
        opponentWon opponent player =
            if isRock opponent && isScissors player then
                ( True, moveValue opponent, moveValue player )

            else if isScissors opponent && isPaper player then
                ( True, moveValue opponent, moveValue player )

            else if isPaper opponent && isRock player then
                ( True, moveValue opponent, moveValue player )

            else
                ( False, moveValue opponent, moveValue player )

        draw : Maybe Move -> Maybe Move -> ( Bool, Int, Int )
        draw opponent player =
            case ( opponent, player ) of
                ( Just value1, Just value2 ) ->
                    case ( value1, value2 ) of
                        ( Rock, Rock ) ->
                            ( True, moveValue opponent, moveValue player )

                        ( Paper, Paper ) ->
                            ( True, moveValue opponent, moveValue player )

                        ( Scissors, Scissors ) ->
                            ( True, moveValue opponent, moveValue player )

                        _ ->
                            ( False, moveValue opponent, moveValue player )

                ( _, _ ) ->
                    ( False, moveValue opponent, moveValue player )

        result : Maybe Move -> Maybe Move -> Scores -> Scores
        result opponent player scores2 =
            let
                ( oppWon, opponentMoveValue, playerMoveValue ) =
                    opponentWon opponent player

                ( drew, opponentMoveValueDrew, playerMoveValueDrew ) =
                    draw opponent player
            in
            if oppWon then
                { scores2 | player = scores2.player + playerMoveValue, opponent = scores2.opponent + opponentMoveValue + 6 }

            else if drew then
                { scores2 | player = scores2.player + playerMoveValueDrew + 3, opponent = scores2.opponent + opponentMoveValueDrew + 3 }

            else
                { scores2 | player = scores2.player + playerMoveValue + 6, opponent = scores2.opponent + opponentMoveValue }
    in
    result (moveCodeOpponent opponentMove) (moveCodePlayer playerMove) scores


isRock : Maybe Move -> Bool
isRock x =
    x == Just Rock


isPaper : Maybe Move -> Bool
isPaper x =
    x == Just Paper


isScissors : Maybe Move -> Bool
isScissors x =
    x == Just Scissors
