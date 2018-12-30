module Interactions exposing (Event(..), Interaction(..), Quality(..), Result(..), addMember, distrnToQuality, equal, getChance, interactionAndQualitiesToResults, resultImpliesResults, resultsFromEvent)

import Array
import Bonds
import Classes
import Quirks
import Random exposing (Generator)
import Random.Float
import Random.List
import Set exposing (Set)
import Stats


type alias Student =
    Int


type Interaction
    = Class (Set Student) Classes.Class
    | Hangout (Set Student)
    | Date (Set Student)
    | Sex (Set Student)
    | Fight (Set Student)
    | Training (Set Student)
    | Party (Set Student)


type Quality
    = Amazing
    | Good
    | Average
    | Bad
    | Awful


type alias Experience =
    ( Student, Quality )


type Result
    = BondsBroken Student (List Bonds.Bond)
    | BondsFormed Student (List Bonds.Bond)
    | StatsChange Student (List Stats.VisibleStats)
    | InvisStatsChange Student (List Stats.InvisibleStats)


type Event
    = HangoutEvent (List Experience) (List Result)


resultImpliesResults : Result -> List Result
resultImpliesResults result =
    let
        bondForms std bond =
            case bond of
                Bonds.InLove x ->
                    [ BondsBroken std [ Bonds.Admires x ], result ]

                Bonds.Admires x ->
                    [ BondsBroken std [ Bonds.Enemy x ], result ]

                Bonds.Enemy x ->
                    [ BondsBroken std [ Bonds.Admires x ], result ]

                _ ->
                    [ result ]
    in
    case result of
        BondsFormed x xs ->
            xs |> List.concatMap (bondForms x)

        _ ->
            [ result ]


resultsFromEvent : Event -> List Result
resultsFromEvent ev =
    case ev of
        HangoutEvent _ xs ->
            xs


filterOutMe : Student -> List Experience -> List Experience
filterOutMe id list =
    list |> List.filter (\( a, b ) -> a /= id)


genBondTarget : List Experience -> Generator (Maybe Student)
genBondTarget exps =
    List.map Tuple.first exps
        |> Random.List.choose
        |> Random.map Tuple.first


interactionAndQualitiesToResults : Interaction -> List Experience -> Generator (List Result)
interactionAndQualitiesToResults int exps =
    let
        expsToBondFormed id constructor =
            exps
                |> filterOutMe id
                |> genBondTarget
                |> Random.map (Maybe.map (\targ -> BondsFormed id [ constructor targ ]))

        expsToBondBroken id constructor =
            exps
                |> filterOutMe id
                |> genBondTarget
                |> Random.map (Maybe.map (\targ -> BondsBroken id [ constructor targ ]))
    in
    case int of
        Hangout _ ->
            exps
                |> List.map
                    (\( id, qual ) ->
                        case qual of
                            Amazing ->
                                Random.andThen identity <|
                                    Random.weighted ( 30, Random.constant Nothing )
                                        [ ( 50, expsToBondFormed id Bonds.Friend )
                                        , ( 20, expsToBondFormed id Bonds.Admires )
                                        , ( 10, expsToBondFormed id Bonds.InLove )
                                        , ( 10, expsToBondFormed id Bonds.Lustful )
                                        ]

                            Good ->
                                Random.andThen identity <|
                                    Random.weighted ( 40, Random.constant Nothing )
                                        [ ( 40, expsToBondFormed id Bonds.Friend )
                                        , ( 10, expsToBondFormed id Bonds.Admires )
                                        , ( 10, expsToBondFormed id Bonds.Rival )
                                        , ( 10, expsToBondFormed id Bonds.Lustful )
                                        ]

                            Average ->
                                Random.andThen identity <|
                                    Random.weighted ( 80, Random.constant Nothing )
                                        [ ( 10, expsToBondFormed id Bonds.Friend )
                                        , ( 5, expsToBondFormed id Bonds.Enemy )
                                        , ( 5, expsToBondFormed id Bonds.Rival )
                                        ]

                            Bad ->
                                Random.andThen identity <|
                                    Random.weighted ( 60, Random.constant Nothing )
                                        [ ( 10, expsToBondFormed id Bonds.Enemy )
                                        , ( 30, expsToBondBroken id Bonds.Friend )
                                        ]

                            Awful ->
                                Random.andThen identity <|
                                    Random.weighted ( 50, expsToBondBroken id Bonds.Friend )
                                        [ ( 50, expsToBondFormed id Bonds.Enemy ) ]
                     {-
                        _ ->
                           Random.constant Nothing
                     -}
                    )
                |> List.foldr
                    (Random.map2
                        (\val2 acc2 ->
                            case val2 of
                                Just x ->
                                    x :: acc2

                                Nothing ->
                                    acc2
                        )
                    )
                    (Random.constant [])

        _ ->
            Random.constant []


distrnToQuality : Float -> Float -> Generator Quality
distrnToQuality mean stddev =
    let
        qualities =
            Array.fromList [ Awful, Bad, Average, Good, Amazing ]
    in
    Random.Float.normal mean stddev
        |> Random.map
            (\x ->
                let
                    selected =
                        if x < 0 then
                            Array.get 0 qualities

                        else if x > 4 then
                            Array.get 4 qualities

                        else
                            Array.get (Basics.round x) qualities
                in
                case selected of
                    Just y ->
                        y

                    Nothing ->
                        Average
            )


type alias StudentInfo =
    { bonds : List Bonds.Bond
    , quirks : List Quirks.Quirk
    }


dateModifier : Bonds.Bond -> Float
dateModifier bond =
    case bond of
        Bonds.Lustful _ ->
            10

        Bonds.InLove _ ->
            15

        Bonds.Friend _ ->
            5

        Bonds.Rival _ ->
            5

        Bonds.Enemy _ ->
            -12

        _ ->
            0


sexModifier : Bonds.Bond -> Float
sexModifier bond =
    case bond of
        Bonds.Lustful _ ->
            20

        Bonds.InLove _ ->
            10

        Bonds.Friend _ ->
            5

        Bonds.Rival _ ->
            5

        Bonds.Enemy _ ->
            15

        _ ->
            0


fightQuirkModifier : Quirks.Quirk -> Float
fightQuirkModifier (Quirks.Quirk quirk) =
    case quirk of
        Quirks.LovesToFight ->
            30

        _ ->
            0


fightModifier : Bonds.Bond -> Float
fightModifier bond =
    case bond of
        Bonds.Friend _ ->
            -5

        Bonds.Rival _ ->
            5

        Bonds.Enemy _ ->
            15

        _ ->
            0


getChance : (Student -> Maybe StudentInfo) -> Interaction -> Float
getChance infoGetter int =
    let
        containsRelative =
            List.any
                (\b ->
                    case b of
                        Bonds.Relative _ _ ->
                            True

                        _ ->
                            False
                )

        getInfoList xs =
            Set.toList xs
                |> List.map infoGetter
                |> List.foldr
                    (\val acc ->
                        case val of
                            Just x ->
                                x :: acc

                            Nothing ->
                                acc
                    )
                    []
    in
    case int of
        Class _ _ ->
            0

        Hangout xs ->
            10 / toFloat (Set.size xs)

        Training xs ->
            10 / toFloat (Set.size xs)

        Fight xs ->
            let
                setInfoList =
                    getInfoList xs
            in
            let
                bondscore =
                    setInfoList
                        |> List.concatMap (\x -> x.bonds)
                        |> List.filter (\y -> Set.member (Bonds.getBondId y) xs)
                        |> List.map dateModifier
                        |> List.foldr (+)
                            0
                        |> (\y -> y / (toFloat (Set.size xs) / 3))

                quirkscore =
                    setInfoList
                        |> List.concatMap (\x -> x.quirks)
                        |> List.map fightQuirkModifier
                        |> List.foldr (+)
                            0
            in
            bondscore + quirkscore

        Party _ ->
            0

        Date xs ->
            let
                bonds =
                    getInfoList xs
                        |> List.concatMap (\x -> x.bonds)
                        |> List.filter (\y -> Set.member (Bonds.getBondId y) xs)
            in
            if containsRelative bonds then
                0

            else
                bonds
                    |> List.map dateModifier
                    |> List.foldr (+)
                        0
                    |> (\y -> y / (toFloat (Set.size xs) / 3))

        Sex xs ->
            let
                bonds =
                    getInfoList xs
                        |> List.concatMap (\x -> x.bonds)
                        |> List.filter (\y -> Set.member (Bonds.getBondId y) xs)
            in
            if containsRelative bonds then
                0

            else
                bonds
                    |> List.map sexModifier
                    |> List.foldr (+)
                        0


addMember : Student -> Interaction -> Interaction
addMember s i =
    case i of
        Class xs b ->
            Class (Set.insert s xs) b

        Hangout xs ->
            Hangout (Set.insert s xs)

        Date xs ->
            Date (Set.insert s xs)

        Sex xs ->
            Sex (Set.insert s xs)

        Fight xs ->
            Fight (Set.insert s xs)

        Training xs ->
            Training (Set.insert s xs)

        Party xs ->
            Party (Set.insert s xs)


equal : Interaction -> Interaction -> Bool
equal a b =
    case ( a, b ) of
        ( Class xs1 c1, Class xs2 c2 ) ->
            (Classes.toString c1 == Classes.toString c2) && (xs1 == xs2)

        ( Hangout xs1, Hangout xs2 ) ->
            xs1 == xs2

        ( Date xs1, Date xs2 ) ->
            xs1 == xs2

        ( Sex xs1, Sex xs2 ) ->
            xs1 == xs2

        ( Fight xs1, Fight xs2 ) ->
            xs1 == xs2

        ( Training xs1, Training xs2 ) ->
            xs1 == xs2

        ( Party xs1, Party xs2 ) ->
            xs1 == xs2

        _ ->
            False
