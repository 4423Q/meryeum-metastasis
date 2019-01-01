module Util exposing
    ( ana
    , combHasHave
    , commasAnd
    , deplural
    , flattenGen
    , genUniformFromArray
    , genUniformFromArray2
    , hasHave
    , isare
    , popNRandom
    , popRand
    , removeNothings
    , slashmode
    , swapGenMaybe
    )

import Array
import Random exposing (Generator)
import String


removeNothings : List (Maybe a) -> List a
removeNothings =
    List.concatMap (Maybe.map List.singleton >> Maybe.withDefault [])


slashmode : { subj : String, obj : String, pos : String, pos2 : String } -> String
slashmode pron =
    .subj pron ++ "/" ++ .obj pron


swapGenMaybe : Maybe (Generator a) -> Generator (Maybe a)
swapGenMaybe mg =
    case mg of
        Just a ->
            a |> Random.map Just

        Nothing ->
            Random.constant Nothing


flattenGen : List (Generator a) -> Generator (List a)
flattenGen gens =
    gens
        |> List.foldr
            (\val acc ->
                Random.map2 (\val2 acc2 -> val2 :: acc2)
                    val
                    acc
            )
            (Random.constant [])


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


combHasHave : String -> String
combHasHave pron =
    case String.toLower pron of
        "they" ->
            "they've"

        x ->
            x ++ "'s"


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


commasAnd : List String -> String
commasAnd list =
    case list of
        [] ->
            ""

        y :: [] ->
            y

        x :: y :: [] ->
            x ++ " and " ++ y

        _ ->
            let
                rev =
                    List.reverse list
            in
            let
                last =
                    List.head rev

                rest =
                    List.tail rev |> Maybe.withDefault []
            in
            Maybe.map
                (\l ->
                    case rest of
                        [] ->
                            l

                        xs ->
                            String.join ", and " [ String.join ", " (List.reverse xs), l ]
                )
                last
                |> Maybe.withDefault ""


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
