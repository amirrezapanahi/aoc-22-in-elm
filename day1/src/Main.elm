module Main exposing (main)

import Html exposing (..)
import List exposing (filterMap)


testInput =
    """
1000
2000
3000

4000

5000
6000

7000
8000
9000

10000
"""


main : Html msg
main =
    input
        |> String.trim
        |> String.split "\n"
        |> elfCalorie
        |> List.map (\innerList -> List.filterMap String.toInt innerList)
        |> List.map (\innerList -> List.sum innerList)
        |> List.sort
        |> List.reverse
        |> List.take 3
        |> List.sum
        |> Debug.toString
        |> Html.text


elfCalorie : List String -> List (List String)
elfCalorie listOfStrings =
    let
        helper : String -> List (List String) -> List (List String)
        helper line listOfLists =
            case listOfLists of
                [] ->
                    [ [ line ] ]

                firstList :: otherList ->
                    if String.isEmpty line then
                        [] :: listOfLists

                    else
                        (firstList ++ [ line ]) :: otherList
    in
    List.foldr
        helper
        []
        listOfStrings
