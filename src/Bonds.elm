module Bonds exposing (Bond(..), Bonds(..), RelativeType(..), add, asList, changeBondId, empty, getBondId, isAssociative, isCommutative, isOfSameType)


type RelativeType
    = Sibling
    | Cousin


type Bond
    = Friend Int
    | InLove Int
    | Lustful Int
    | Enemy Int
    | Rival Int
    | Relative RelativeType Int


isCommutative : Bond -> Bool
isCommutative bond =
    case bond of
        Friend _ ->
            True

        InLove _ ->
            False

        Enemy _ ->
            False

        Lustful _ ->
            False

        Rival _ ->
            True

        Relative _ _ ->
            True


isAssociative : Bond -> Bool
isAssociative bond =
    case bond of
        Friend _ ->
            False

        InLove _ ->
            False

        Enemy _ ->
            False

        Rival _ ->
            False

        Lustful _ ->
            False

        Relative Sibling _ ->
            True

        Relative _ _ ->
            False


isOfSameType : Bond -> Bond -> Bool
isOfSameType a b =
    case ( a, b ) of
        ( Friend _, Friend _ ) ->
            True

        ( InLove _, InLove _ ) ->
            True

        ( Enemy _, Enemy _ ) ->
            True

        ( Rival _, Rival _ ) ->
            True

        ( Lustful _, Lustful _ ) ->
            True

        ( Relative Sibling _, Relative Sibling _ ) ->
            True

        ( Relative Cousin _, Relative Cousin _ ) ->
            True

        --( Relative Twin _, Relative Twin _ ) ->
        --   True
        _ ->
            False


getBondId : Bond -> Int
getBondId bond =
    case bond of
        Friend x ->
            x

        InLove x ->
            x

        Enemy x ->
            x

        Rival x ->
            x

        Relative _ x ->
            x

        Lustful x ->
            x


changeBondId : Int -> Bond -> Bond
changeBondId id bond =
    case bond of
        Friend _ ->
            Friend id

        InLove _ ->
            InLove id

        Lustful _ ->
            Lustful id

        Enemy _ ->
            Enemy id

        Rival _ ->
            Rival id

        Relative y x ->
            Relative y id


type Bonds
    = Bonds (List Bond)


empty : Bonds
empty =
    Bonds []


asList : Bonds -> List Bond
asList bonds =
    case bonds of
        Bonds x ->
            x


add : Bond -> Bonds -> Bonds
add bond bonds =
    Bonds (bond :: asList bonds)
