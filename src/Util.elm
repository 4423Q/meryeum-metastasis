module Util exposing (ana, deplural, genUniformFromArray, genUniformFromArray2, hasHave, isare, popNRandom, popRand)

import Array
import Random exposing (Generator)
import String


popRand : ( Float, a ) -> List ( Float, a ) -> Generator ( a, List ( Float, a ) )
popRand first rest =
    Random.weighted first rest
        |> Random.map
            (\choice ->
                ( choice, List.filter (\x -> Tuple.second x /= choice) (first :: rest) )
            )


popNRandom : Int -> List ( Float, a ) -> Generator (List a)
popNRandom n ints =
    case n of
        0 ->
            Random.constant []

        _ ->
            case ints of
                [] ->
                    Random.constant []

                x :: xs ->
                    popRand x xs
                        |> Random.andThen
                            (\( val, newList ) ->
                                popNRandom (n - 1) newList
                                    |> Random.map
                                        (\otherPicks -> val :: otherPicks)
                            )


isare : String -> String
isare pron =
    case String.toLower pron of
        "they" ->
            "are"

        _ ->
            "is"


hasHave : String -> String
hasHave pron =
    case String.toLower pron of
        "they" ->
            "have"

        _ ->
            "has"


deplural : String -> String -> String
deplural pron word =
    case String.toLower pron of
        "they" ->
            case String.endsWith "s" word of
                True ->
                    String.dropRight 1 word

                False ->
                    word

        _ ->
            case
                String.endsWith "s" word
            of
                True ->
                    word

                False ->
                    String.append word "s"


ana : String -> String
ana word =
    let
        checks =
            [ "a", "e", "i", "o", "u", "h" ]

        starter =
            String.left 1 (String.toLower word)
    in
    if List.member starter checks then
        "an"

    else
        "a"


genUniformFromArray2 : Array.Array a -> Generator (Maybe a)
genUniformFromArray2 arr =
    Random.map
        (\item -> Array.get item arr)
        (Random.int
            0
            (Array.length arr - 1)
        )


genUniformFromArray : (a -> b) -> a -> Array.Array a -> Generator b
genUniformFromArray func default arr =
    Random.map
        (\item ->
            case Array.get item arr of
                Just x ->
                    func x

                Nothing ->
                    func default
        )
        (Random.int
            0
            (Array.length arr - 1)
        )
