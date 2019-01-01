module Pronouns exposing
    ( Pronoun(..)
    , asRecord
    , fromRecord
    , genDiffPronouns
    , newProns
    , obj
    , pos
    , pos2
    , pronounsList
    , slashmode
    , subj
    )

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


subj (Pronoun a _ _ _) =
    a


obj (Pronoun _ a _ _) =
    a


pos (Pronoun _ _ a _) =
    a


pos2 (Pronoun _ _ _ a) =
    a


slashmode pron =
    subj pron ++ "/" ++ obj pron


asRecord : Pronoun -> { subj : String, obj : String, pos : String, pos2 : String }
asRecord (Pronoun a b c d) =
    { subj = a, obj = b, pos = c, pos2 = d }


fromRecord : { subj : String, obj : String, pos : String, pos2 : String } -> Pronoun
fromRecord p =
    Pronoun p.subj p.obj p.pos p.pos2


genDiffPronouns : Pronoun -> Generator Pronoun
genDiffPronouns p =
    let
        newPList =
            pronounsList
                |> List.filter (Tuple.second >> (/=) p)
    in
    Random.weighted
        (case List.head newPList of
            Just x ->
                x

            Nothing ->
                ( 30, Pronoun "they" "them" "their" "their" )
        )
        (case List.tail newPList of
            Just x ->
                x

            Nothing ->
                []
        )


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
