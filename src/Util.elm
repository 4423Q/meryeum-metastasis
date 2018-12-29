module Util exposing (ana, genUniformFromArray, genUniformFromArray2, isare)

import Array
import Random exposing (Generator)
import String


isare : String -> String
isare pron =
    case String.toLower pron of
        "they" ->
            "are"

        _ ->
            "is"


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
