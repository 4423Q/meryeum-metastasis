module Pronouns exposing (Pronoun(..), newProns, pronounsList)

import Random exposing (Generator)



-- They/Them/Their/Their - She/Her/Her/Hers


type Pronoun
    = Pronoun String String String String


pronounsList =
    [ ( 30, Pronoun "they" "them" "their" "their" )
    , ( 30, Pronoun "he" "him" "his" "his" )
    , ( 30, Pronoun "she" "her" "her " "hers" )
    , ( 10, Pronoun "ze" "zir" "zir" "zirs" )
    , ( 10, Pronoun "ze" "hir" "hir" "hirs" )
    , ( 10, Pronoun "xe" "xem" "xir" "xirs" )
    ]


newProns : Generator Pronoun
newProns =
    Random.weighted
        (case List.head pronounsList of
            Just x ->
                x

            Nothing ->
                ( 30, Pronoun "they" "them" "their" "their" )
        )
        (case List.tail pronounsList of
            Just x ->
                x

            Nothing ->
                []
        )
