module Bonds exposing
    ( Bond(..)
    , Bonds(..)
    , RelativeType(..)
    , add
    , asList
    , changeBondId
    , checkBondIsValidToAdd
    , empty
    , fromList
    , getBondId
    , isAssociative
    , isCommutative
    , isOfSameType
    , isTargetCompatible
    , randomNonRelative
    )


type RelativeType
    = Sibling
    | Cousin


type Bond
    = Friend Int
    | InLove Int
    | Lustful Int
    | Admires Int
    | Enemy Int
    | Rival Int
    | Relative RelativeType Int


type alias Weighted a =
    ( Float, a )


checkBondIsValidToAdd : List Bond -> List Bond -> Bond -> Int -> Bool
checkBondIsValidToAdd sourceBonds targetBonds bond sourceId =
    not (List.member bond sourceBonds)
        && isTargetCompatible targetBonds bond sourceId
        && not (getBondId bond == sourceId)


randomNonRelative : Int -> ( Weighted Bond, List (Weighted Bond) )
randomNonRelative a =
    ( ( 50, Friend a )
    , [ ( 30, Enemy a )
      , ( 20, Rival a )
      , ( 15, Admires a )
      , ( 5, Lustful a )
      , ( 5, InLove a )
      ]
    )


isTargetCompatible : List Bond -> Bond -> Int -> Bool
isTargetCompatible targetBonds bond sourceId =
    let
        isRelative =
            List.any
                (\b ->
                    case b of
                        Relative _ source ->
                            source == sourceId

                        _ ->
                            False
                )
                targetBonds
    in
    case bond of
        InLove _ ->
            not isRelative

        Lustful _ ->
            not isRelative

        _ ->
            True


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

        Admires _ ->
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

        Admires _ ->
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

        ( Admires _, Admires _ ) ->
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

        Admires x ->
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

        Admires _ ->
            Admires id

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


fromList : List Bond -> Bonds
fromList bonds =
    Bonds bonds


asList : Bonds -> List Bond
asList bonds =
    case bonds of
        Bonds x ->
            x


add : Bond -> Bonds -> Bonds
add bond bonds =
    Bonds (bond :: asList bonds)
