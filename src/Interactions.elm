module Interactions exposing
    ( Event(..)
    , Interaction(..)
    , Quality(..)
    , Result(..)
    , StudentInfo
    , Transition(..)
    , addMember
    , buildDate
    , buildHangout
    , distrnToQuality
    , equal
    , fromBond
    , genRandomEvent
    , getChance
    , getResultTarget
    , interactionAndQualitiesToResults
    , interactionsToEvents
    , qualityToString
    , resolveInteraction
    , resultImpliesResults
    , resultsFromEvent
    )

import Array
import Bonds
import Classes
import List.Extra
import Name
import Pronouns
import Quirks
import Random exposing (Generator)
import Random.Float
import Random.List
import Set exposing (Set)
import Stats
import Util


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


getResultTarget : Result -> Student
getResultTarget res =
    case res of
        BondsBroken x _ ->
            x

        BondsFormed x _ ->
            x

        StatsChange x _ ->
            x

        InvisStatsChange x _ ->
            x

        NameChange x _ ->
            x

        PronounChange x _ ->
            x


type Result
    = BondsBroken Student (List Bonds.Bond)
    | BondsFormed Student (List Bonds.Bond)
    | StatsChange Student Stats.VisibleStats
    | InvisStatsChange Student Stats.InvisibleStats
    | NameChange Student Name.Name
    | PronounChange Student Pronouns.Pronoun


type Transition
    = NameT Name.Name
    | PronT Pronouns.Pronoun
    | BothT Name.Name Pronouns.Pronoun


type Event
    = HangoutEvent (List Experience) (List Result)
    | DateEvent (List Experience) (List Result)
    | TransitionEvent Student Transition (List Result)
    | ClassEvent Classes.Class (List Result)


type alias StudentInfo =
    { id : Student
    , bonds : List Bonds.Bond
    , quirks : List Quirks.Quirk
    , name : Name.Name
    , pronouns : Pronouns.Pronoun
    , visible : Stats.VisibleStats
    , invisible : Stats.InvisibleStats
    }


type alias InfoGetter =
    Student -> Maybe StudentInfo



-- QUality functions


qualityToString : Quality -> String
qualityToString qual =
    case qual of
        Amazing ->
            "amazing"

        Good ->
            "good"

        Average ->
            "average"

        Bad ->
            "bad"

        Awful ->
            "awful"



-- Functions for Interaction type


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



-- Useful processing functions


areRelatives : StudentInfo -> StudentInfo -> Bool
areRelatives a b =
    a.bonds
        |> List.any
            (\bond ->
                case bond of
                    Bonds.Relative _ x ->
                        x == b.id

                    _ ->
                        False
            )


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

                Bonds.Friend x ->
                    [ BondsBroken std [ Bonds.Enemy x ], result ]

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

        DateEvent _ xs ->
            xs

        TransitionEvent _ _ xs ->
            xs

        ClassEvent _ xs ->
            xs


filterOutMe : Student -> List Experience -> List Experience
filterOutMe id list =
    list |> List.filter (\( a, b ) -> a /= id)


genBondTarget : List Experience -> Generator (Maybe Student)
genBondTarget exps =
    List.map Tuple.first exps
        |> Random.List.choose
        |> Random.map Tuple.first


resultsFromClass : InfoGetter -> Set Student -> Generator (List Result)
resultsFromClass infoGetter students =
    let
        participants =
            Set.toList students
                |> List.map (\x -> infoGetter x |> Maybe.map (\y -> ( x, y )))
                |> Util.removeNothings
    in
    let
        everyelse id =
            participants
                |> List.filter (Tuple.first >> (/=) id)

        hottest =
            -- Sort in inverse order so we get the hottest and not the least hot
            List.take 5 (List.sortBy (\( _, y ) -> 10 - y.visible.hot) participants)

        lustfulChance targetInfo sourceInfo =
            if areRelatives targetInfo sourceInfo then
                0

            else if List.member Quirks.ExtraHot targetInfo.quirks then
                10000

            else
                (toFloat (targetInfo.visible.hot * sourceInfo.invisible.horny) / 25) * 0.05

        sharpest =
            List.take 5 (List.sortBy (\( _, y ) -> 10 - y.visible.sharp) participants)

        admirationChance targetInfo sourceInfo =
            (toFloat targetInfo.visible.sharp / 5) * 0.2

        mostfun =
            List.take 5
                (List.sortBy
                    (\( _, y ) ->
                        10 - y.invisible.diversion
                    )
                    participants
                )

        howmanytotake info =
            clamp 1 10 ((info.visible.extra - 2) // 2)
    in
    Random.map List.concat <|
        Util.flattenGen
            [ hottest
                |> List.map
                    (\( id, info ) ->
                        everyelse id
                            |> List.take (howmanytotake info)
                            |> List.map
                                (\( sId, sInfo ) ->
                                    Random.weighted
                                        ( 1, Nothing )
                                        [ ( lustfulChance info sInfo, Just (BondsFormed sId [ Bonds.Lustful id ]) ) ]
                                )
                            |> Util.flattenGen
                    )
                |> Util.flattenGen
                |> Random.map List.concat
                |> Random.map Util.removeNothings
            , sharpest
                |> List.map
                    (\( id, info ) ->
                        everyelse id
                            |> List.take (howmanytotake info)
                            |> List.map
                                (\( sId, sInfo ) ->
                                    Random.weighted
                                        ( 1, Nothing )
                                        [ ( admirationChance info sInfo, Just (BondsFormed sId [ Bonds.Admires id ]) ) ]
                                )
                            |> Util.flattenGen
                    )
                |> Util.flattenGen
                |> Random.map List.concat
                |> Random.map Util.removeNothings
            ]


resolveInteraction : InfoGetter -> Interaction -> Generator (List Event)
resolveInteraction infoGetter int =
    case int of
        Class xs c ->
            resultsFromClass infoGetter xs
                |> Random.map (ClassEvent c >> List.singleton)

        Date xs ->
            Random.list (Set.size xs) (distrnToQuality 2 1)
                |> Random.andThen
                    (\qualities ->
                        let
                            quals =
                                List.Extra.zip (Set.toList xs) qualities
                        in
                        interactionAndQualitiesToResults int quals
                            |> Random.map
                                (DateEvent quals
                                    >> List.singleton
                                )
                    )

        Hangout xs ->
            Random.list (Set.size xs) (distrnToQuality 2 1)
                |> Random.andThen
                    (\qualities ->
                        let
                            quals =
                                List.Extra.zip (Set.toList xs) qualities
                        in
                        interactionAndQualitiesToResults int quals
                            |> Random.map
                                (HangoutEvent quals >> List.singleton)
                    )

        _ ->
            Random.constant []


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

        processExps func =
            exps
                |> List.map func
                |> List.map (Random.andThen identity)
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
    in
    case int of
        Date _ ->
            processExps
                (\( id, qual ) ->
                    case qual of
                        Amazing ->
                            Random.weighted ( 30, Random.constant Nothing )
                                [ ( 30, expsToBondFormed id Bonds.Admires )
                                , ( 30, expsToBondFormed id Bonds.Lustful )
                                , ( 20, expsToBondFormed id Bonds.InLove )
                                ]

                        _ ->
                            Random.constant (Random.constant Nothing)
                )

        Hangout _ ->
            processExps
                (\( id, qual ) ->
                    case qual of
                        Amazing ->
                            Random.weighted ( 30, Random.constant Nothing )
                                [ ( 50, expsToBondFormed id Bonds.Friend )
                                , ( 20, expsToBondFormed id Bonds.Admires )
                                , ( 10, expsToBondFormed id Bonds.InLove )
                                , ( 10, expsToBondFormed id Bonds.Lustful )
                                ]

                        Good ->
                            Random.weighted ( 40, Random.constant Nothing )
                                [ ( 40, expsToBondFormed id Bonds.Friend )
                                , ( 10, expsToBondFormed id Bonds.Admires )
                                , ( 10, expsToBondFormed id Bonds.Rival )
                                , ( 10, expsToBondFormed id Bonds.Lustful )
                                ]

                        Average ->
                            Random.weighted ( 80, Random.constant Nothing )
                                [ ( 10, expsToBondFormed id Bonds.Friend )
                                , ( 5, expsToBondFormed id Bonds.Enemy )
                                , ( 5, expsToBondFormed id Bonds.Rival )
                                ]

                        Bad ->
                            Random.weighted ( 60, Random.constant Nothing )
                                [ ( 10, expsToBondFormed id Bonds.Enemy )
                                , ( 30, expsToBondBroken id Bonds.Friend )
                                ]

                        Awful ->
                            Random.weighted ( 50, expsToBondBroken id Bonds.Friend )
                                [ ( 50, expsToBondFormed id Bonds.Enemy ) ]
                )

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
fightQuirkModifier quirk =
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


buildRecursiveInteraction : (StudentInfo -> StudentInfo -> Bool) -> (Bonds.Bond -> Bool) -> (Set Student -> Interaction) -> InfoGetter -> Student -> Student -> List Int -> List Interaction -> ( List Int, List Interaction )
buildRecursiveInteraction bondsCompatible targBondFilter intBuilder infoGetter sourceId targetId handledSources existing =
    Maybe.withDefault ( handledSources, existing ) <|
        Maybe.map2
            (\source target ->
                let
                    targBonds =
                        target.bonds

                    initialInteraction =
                        intBuilder
                            (Set.insert
                                sourceId
                                (Set.singleton targetId)
                            )
                in
                if List.member sourceId handledSources then
                    ( handledSources, existing )

                else if not (bondsCompatible source target) then
                    ( sourceId :: handledSources, existing )

                else if List.any (equal initialInteraction) existing then
                    ( sourceId :: handledSources, existing )

                else
                    targBonds
                        |> List.filter (\x -> Bonds.getBondId x /= sourceId)
                        |> List.filter targBondFilter
                        |> List.foldr
                            (\val ( handled, acc ) ->
                                let
                                    target2 =
                                        Bonds.getBondId val
                                in
                                buildRecursiveInteraction bondsCompatible targBondFilter intBuilder infoGetter targetId target2 handled acc
                                    |> Tuple.mapSecond (List.map (addMember sourceId))
                            )
                            ( sourceId :: handledSources, initialInteraction :: existing )
            )
            (infoGetter sourceId)
            (infoGetter targetId)


buildDate : InfoGetter -> Student -> Student -> List Interaction -> List Interaction
buildDate infogetter sourceId targetId existing =
    buildRecursiveInteraction (\x y -> areRelatives x y |> not) (Bonds.isOfSameType (Bonds.InLove 0)) Date infogetter sourceId targetId [] existing
        |> Tuple.second


buildHangout : InfoGetter -> Student -> Student -> List Int -> List Interaction -> ( List Int, List Interaction )
buildHangout =
    buildRecursiveInteraction
        (\x y -> True)
        (\x ->
            case x of
                Bonds.Friend _ ->
                    True

                Bonds.Relative _ _ ->
                    True

                _ ->
                    False
        )
        Hangout


fromBond : (Student -> Maybe StudentInfo) -> Student -> Bonds.Bond -> List Interaction
fromBond infoGetter sId bond =
    let
        bd =
            \x -> buildDate infoGetter sId x []

        bh =
            \x -> buildHangout infoGetter sId x [] [] |> Tuple.second
    in
    case bond of
        Bonds.InLove x ->
            bd x

        Bonds.Friend x ->
            bh x

        Bonds.Admires x ->
            List.append (bh x) (bd x)

        Bonds.Rival x ->
            [ Fight (Set.fromList [ x, sId ])
            , Training (Set.fromList [ x, sId ])
            ]

        Bonds.Lustful x ->
            bd x

        _ ->
            []


interactionsToEvents : InfoGetter -> List Interaction -> Generator (List Event)
interactionsToEvents infoGetter ints =
    Random.List.shuffle ints
        |> Random.map
            (List.map
                (\y ->
                    resolveInteraction infoGetter y
                        |> Random.map (List.map Random.constant)
                )
            )
        |> Random.andThen Util.flattenGen
        |> Random.map List.concat
        |> Random.andThen Util.flattenGen


genRandomEvent : InfoGetter -> Student -> Generator (Maybe Event)
genRandomEvent infoGetter student =
    Random.andThen identity <|
        Random.weighted
            ( 99, Random.constant Nothing )
            [ ( 1
              , Random.int 0 2
                    |> Random.andThen
                        (\x ->
                            infoGetter student
                                |> Maybe.map
                                    (\s ->
                                        case x of
                                            2 ->
                                                Random.map2
                                                    (\name pro ->
                                                        TransitionEvent student
                                                            (BothT name pro)
                                                            [ NameChange student name
                                                            , PronounChange student pro
                                                            ]
                                                    )
                                                    (Name.newRelativeName s.name)
                                                    (Pronouns.genDiffPronouns s.pronouns)

                                            1 ->
                                                Random.map
                                                    (\name ->
                                                        TransitionEvent student
                                                            (NameT name)
                                                            [ NameChange student name ]
                                                    )
                                                    (Name.newRelativeName s.name)

                                            _ ->
                                                Random.map
                                                    (\pron ->
                                                        TransitionEvent student
                                                            (PronT pron)
                                                            [ PronounChange student pron ]
                                                    )
                                                    (Pronouns.genDiffPronouns s.pronouns)
                                    )
                                |> (\y ->
                                        case y of
                                            Just z ->
                                                Random.constant (Just z)

                                            Nothing ->
                                                Random.constant Nothing
                                   )
                        )
                    |> Random.andThen Util.swapGenMaybe
              )
            ]


getChance : InfoGetter -> Interaction -> Float
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
