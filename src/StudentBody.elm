module StudentBody exposing
    ( StudentBody
    , addRelative
    , addStudent
    , addStudents
    , asList
    , empty
    , findAllInteractions
    , findInteractions
    , genStudent
    , genWeeksInteractions
    , getBondTarget
    , getNum
    , getStudentById
    , resolveInteractions
    )

import Array
import Bonds
import Classes
import Debug
import Interactions
import List.Extra
import Random exposing (Generator)
import Random.Float
import Random.List
import Set
import Student exposing (newStudent)
import Util



-- StudentBody has a list of students and the current largest student number


type StudentBody
    = StudentBody (List Student.Student) Int


asList : StudentBody -> List Student.Student
asList sBody =
    case sBody of
        StudentBody x _ ->
            x


getNum : StudentBody -> Int
getNum sBody =
    case sBody of
        StudentBody _ x ->
            x


empty : StudentBody
empty =
    StudentBody [] 0


getStudentById : StudentBody -> Int -> Maybe Student.Student
getStudentById body id =
    case body of
        StudentBody x _ ->
            x |> List.filter (\y -> Student.getNumber y == id) |> List.head


updateStudentById : (Student.Student -> Student.Student) -> StudentBody -> Int -> Maybe StudentBody
updateStudentById updateFunc body id =
    case body of
        StudentBody x num ->
            case
                getStudentById body id
            of
                Just student ->
                    Just (StudentBody (updateFunc student :: List.filter (\y -> Student.getNumber y /= id) x) num)

                Nothing ->
                    Nothing



-- recusiveIntBuilder : (Set Int -> Interactions.Interaction) ->  (Bonds.Bond -> Bool) -> Student.Student -> Student.Student -> StudentBody -> List Interactions.Interaction -> List Interactions.Interaction


buildHangout : Student.Student -> Student.Student -> StudentBody -> List Int -> List Interactions.Interaction -> ( List Int, List Interactions.Interaction )
buildHangout source target body handledSources existing =
    let
        targBonds =
            Student.getBonds target

        sourceId =
            Student.getNumber source

        targetId =
            Student.getNumber target

        initialInteraction =
            Interactions.Hangout
                (Set.insert
                    (Student.getNumber source)
                    (Set.singleton (Student.getNumber target))
                )
    in
    if List.member sourceId handledSources then
        ( handledSources, existing )

    else if List.any (Interactions.equal initialInteraction) existing || List.length existing > 4 then
        ( sourceId :: handledSources, existing )

    else
        let
            combined =
                initialInteraction :: existing

            newHandledSources =
                sourceId :: handledSources
        in
        case
            targBonds
                |> List.filter
                    (\x ->
                        (case x of
                            Bonds.Friend _ ->
                                True

                            Bonds.Relative _ _ ->
                                True

                            _ ->
                                False
                        )
                            && Bonds.getBondId x
                            /= sourceId
                    )
        of
            [] ->
                ( newHandledSources, combined )

            xs ->
                xs
                    |> List.foldr
                        (\val ( handled, acc ) ->
                            let
                                target2 =
                                    getStudentById body (Bonds.getBondId val)
                            in
                            case target2 of
                                Just t2 ->
                                    buildHangout target t2 body newHandledSources acc
                                        |> Tuple.mapSecond (List.map (Interactions.addMember sourceId))

                                Nothing ->
                                    ( handled, acc )
                        )
                        ( newHandledSources, combined )


buildDate : Student.Student -> Student.Student -> StudentBody -> List Interactions.Interaction -> List Interactions.Interaction
buildDate source target body existing =
    let
        targBonds =
            Student.getBonds target

        sourceId =
            Student.getNumber source

        targetId =
            Student.getNumber target

        initialInteraction =
            Interactions.Date
                (Set.insert
                    (Student.getNumber source)
                    (Set.singleton (Student.getNumber target))
                )
    in
    if List.member initialInteraction existing then
        []

    else
        let
            combined =
                initialInteraction :: existing
        in
        case targBonds |> List.filter (\x -> Bonds.getBondId x /= sourceId) of
            [] ->
                combined

            xs ->
                xs
                    |> List.filter (Bonds.isOfSameType (Bonds.InLove 0))
                    |> List.foldr
                        (\val acc ->
                            let
                                target2 =
                                    getStudentById body (Bonds.getBondId val)
                            in
                            case target2 of
                                Just t2 ->
                                    buildDate target t2 body acc
                                        |> List.map (Interactions.addMember sourceId)

                                Nothing ->
                                    acc
                        )
                        combined


interactionsFromBond :
    Bonds.Bond
    -> Student.Student
    -> StudentBody
    -> List Interactions.Interaction
interactionsFromBond bond sS body =
    case bond of
        Bonds.InLove x ->
            let
                target =
                    getStudentById body x
            in
            case target of
                Nothing ->
                    []

                Just tS ->
                    buildDate sS tS body []

        Bonds.Friend x ->
            let
                target =
                    getStudentById body x
            in
            case target of
                Nothing ->
                    []

                Just tS ->
                    case
                        buildHangout sS tS body [] []
                    of
                        ( _, v ) ->
                            v

        Bonds.Rival x ->
            let
                target =
                    getStudentById body x
            in
            case target of
                Nothing ->
                    []

                Just tS ->
                    [ Interactions.Fight (Set.fromList [ x, Student.getNumber sS ])
                    , Interactions.Training (Set.fromList [ x, Student.getNumber sS ])
                    ]

        _ ->
            []


findInteractions : Student.Student -> StudentBody -> List Interactions.Interaction
findInteractions student body =
    Student.getBonds student
        |> List.map (\x -> interactionsFromBond x student body)
        |> List.concat


findAllInteractions : StudentBody -> List Interactions.Interaction
findAllInteractions body =
    let
        bodyList =
            asList body
    in
    let
        classes =
            List.Extra.uniqueBy Classes.toString (List.concatMap Student.getClasses bodyList)
    in
    List.concat
        [ bodyList
            |> List.concatMap (\x -> findInteractions x body)
        , classes
            |> List.map
                (\class ->
                    Interactions.Class
                        (bodyList
                            |> List.filter (\x -> List.member class (Student.getClasses x))
                            |> List.map Student.getNumber
                            |> Set.fromList
                        )
                        class
                )
        ]


popRandInteraction : ( Float, Interactions.Interaction ) -> List ( Float, Interactions.Interaction ) -> Generator ( Interactions.Interaction, List ( Float, Interactions.Interaction ) )
popRandInteraction int ints =
    Random.weighted int ints
        |> Random.map
            (\choice ->
                ( choice, List.filter (\x -> Tuple.second x /= choice) ints )
            )


pickNInteractions : Int -> List ( Float, Interactions.Interaction ) -> Generator (List Interactions.Interaction)
pickNInteractions n ints =
    case n of
        0 ->
            Random.constant []

        _ ->
            case ints of
                [] ->
                    Random.constant []

                x :: xs ->
                    popRandInteraction x xs
                        |> Random.andThen
                            (\( val, newList ) ->
                                pickNInteractions (n - 1) newList
                                    |> Random.map
                                        (\otherPicks -> val :: otherPicks)
                            )


resolveInteractions : StudentBody -> List Interactions.Interaction -> Generator ( List Interactions.Event, StudentBody )
resolveInteractions body ints =
    -- Eventually do a thing to decide how interactions go based on stats, but for now fuck it
    let
        processInteraction =
            \val acc ->
                acc
                    |> Random.andThen
                        (\( events, newbody ) ->
                            case val of
                                Interactions.Hangout xs ->
                                    Random.list (Set.size xs) (Interactions.distrnToQuality 2 1)
                                        |> Random.andThen
                                            (\qualities ->
                                                let
                                                    quals =
                                                        List.Extra.zip (Set.toList xs) qualities
                                                in
                                                Interactions.interactionAndQualitiesToResults val quals
                                                    |> Random.map
                                                        (\results ->
                                                            ( Interactions.HangoutEvent quals results :: events, newbody )
                                                        )
                                            )

                                _ ->
                                    Random.constant ( events, newbody )
                        )
    in
    Random.List.shuffle ints
        |> Random.andThen (List.foldr processInteraction (Random.constant ( [], body )))


genWeeksInteractions : StudentBody -> Generator (List Interactions.Interaction)
genWeeksInteractions body =
    let
        possibleInteractions =
            findAllInteractions body

        isClassInt =
            \x ->
                case x of
                    Interactions.Class _ _ ->
                        True

                    _ ->
                        False

        infoGetter =
            \id ->
                getStudentById body id
                    |> Maybe.map
                        (\std ->
                            { bonds = Student.getBonds std
                            , quirks = Student.getQuirks std
                            }
                        )
    in
    let
        classInteractions =
            possibleInteractions |> List.filter isClassInt

        otherInteractions =
            possibleInteractions |> List.filter (\x -> not <| isClassInt x) |> List.map (\x -> ( Interactions.getChance infoGetter x, x ))
    in
    pickNInteractions 10 otherInteractions
        |> Random.map
            (\x -> List.append x classInteractions)


addStudent : Student.Student -> StudentBody -> ( Student.Student, StudentBody )
addStudent student (StudentBody students lastNum) =
    let
        nstudent =
            Student.setNumber (lastNum + 1) student
    in
    ( nstudent, StudentBody (nstudent :: students) (lastNum + 1) )


genStudent : StudentBody -> Generator (Maybe ( Student.Student, StudentBody ))
genStudent ((StudentBody students lastNum) as studentBody) =
    Random.andThen identity <|
        Random.weighted
            ( 85
            , newStudent
                |> Random.andThen
                    (\student ->
                        Random.map
                            (\x -> ( student, x ))
                            (Random.uniform True [ False ])
                    )
                |> Random.andThen
                    (\( x, shouldHaveABond ) ->
                        case shouldHaveABond of
                            False ->
                                Random.constant (Just (addStudent x studentBody))

                            True ->
                                let
                                    ( newStd, newBody ) =
                                        addStudent x studentBody
                                in
                                Random.map
                                    (\bond ->
                                        addBondToStudentById bond (Student.getNumber newStd) newBody
                                            |> Maybe.andThen
                                                (\newBody2 ->
                                                    case
                                                        getStudentById newBody (Student.getNumber newStd)
                                                    of
                                                        Just a ->
                                                            Just ( a, newBody2 )

                                                        Nothing ->
                                                            Nothing
                                                )
                                    )
                                    (genRandomNonRelativeBond newBody)
                    )
            )
            [ ( 15, genRandomRelative studentBody ) ]


genRandomNonRelativeBond : StudentBody -> Generator Bonds.Bond
genRandomNonRelativeBond body =
    Util.genUniformFromArray2
        (Array.fromList (asList body))
        |> Random.andThen
            (\x ->
                case x of
                    Just b ->
                        let
                            a =
                                Student.getNumber b
                        in
                        Random.weighted
                            ( 50, Bonds.Friend a )
                            [ ( 30, Bonds.Enemy a )
                            , ( 20, Bonds.Rival a )
                            , ( 10, Bonds.Lustful a )
                            , ( 5, Bonds.InLove a )
                            ]

                    Nothing ->
                        genRandomNonRelativeBond body
            )


addStudents : Int -> StudentBody -> Generator StudentBody
addStudents n studentBody =
    case n of
        0 ->
            Random.constant studentBody

        _ ->
            Random.andThen identity <|
                Random.map
                    (\res ->
                        case res of
                            Just ( _, newBody ) ->
                                addStudents (n - 1) newBody

                            _ ->
                                addStudents n studentBody
                    )
                    (genStudent studentBody)


addBondToStudentById : Bonds.Bond -> Int -> StudentBody -> Maybe StudentBody
addBondToStudentById bond id body =
    case Bonds.getBondId bond == id of
        True ->
            Just body

        False ->
            getStudentById body id
                |> Maybe.map
                    (\s ->
                        Student.getBonds s
                            |> List.any ((==) bond)
                    )
                |> Maybe.andThen
                    (\hasBondAlready ->
                        case hasBondAlready of
                            True ->
                                Just body

                            False ->
                                let
                                    comm =
                                        Bonds.isCommutative bond

                                    ass =
                                        Bonds.isAssociative bond

                                    target =
                                        Bonds.getBondId bond

                                    updatedStudentBody =
                                        updateStudentById (Student.addBond bond) body id
                                in
                                case comm of
                                    False ->
                                        updatedStudentBody

                                    True ->
                                        updatedStudentBody
                                            |> Maybe.andThen
                                                (\usb2 -> updateStudentById (Student.addBond (Bonds.changeBondId id bond)) usb2 target)
                                            |> Maybe.andThen
                                                (\suBody ->
                                                    case ass of
                                                        False ->
                                                            Just suBody

                                                        --Implement this
                                                        True ->
                                                            getStudentById suBody target
                                                                |> Maybe.map
                                                                    (\targetStudent -> Student.getBonds targetStudent)
                                                                |> Maybe.map
                                                                    (List.filter (Bonds.isOfSameType bond))
                                                                |> Maybe.andThen
                                                                    (List.foldr
                                                                        (\val ->
                                                                            Maybe.andThen
                                                                                (\b ->
                                                                                    addBondToStudentById (Bonds.changeBondId id bond)
                                                                                        (Bonds.getBondId val)
                                                                                        b
                                                                                )
                                                                        )
                                                                        (Just suBody)
                                                                    )
                                                )
                    )


genRandomRelative : StudentBody -> Generator (Maybe ( Student.Student, StudentBody ))
genRandomRelative body =
    Util.genUniformFromArray2
        (Array.fromList (asList body))
        |> Random.andThen
            (\x ->
                case x of
                    Just a ->
                        genRelative a body

                    Nothing ->
                        Random.constant Nothing
            )


genRelative : Student.Student -> StudentBody -> Generator (Maybe ( Student.Student, StudentBody ))
genRelative target body =
    let
        targetNumber =
            Student.getNumber target
    in
    Random.uniform (Bonds.Relative Bonds.Sibling targetNumber)
        [ Bonds.Relative Bonds.Cousin targetNumber ]
        |> Random.andThen
            (\bond ->
                (case bond of
                    Bonds.Relative Bonds.Sibling _ ->
                        Random.constant (Student.newRelative target)

                    _ ->
                        Random.uniform (Student.newRelative target) [ Student.newStudent ]
                )
                    |> Random.andThen identity
                    |> Random.map (\student -> ( student, bond ))
            )
        |> Random.map
            (\( student, bond ) ->
                let
                    ( addedStudent, newBody ) =
                        addStudent student body
                in
                addBondToStudentById bond (Student.getNumber addedStudent) newBody
                    |> Maybe.map
                        (\body2 -> ( addedStudent, body2 ))
            )


addRelative : Int -> StudentBody -> Maybe (Generator (Maybe StudentBody))
addRelative n ((StudentBody students lastNum) as studentBody) =
    Maybe.map
        (\target ->
            genRelative target studentBody
                |> Random.map (Maybe.map Tuple.second)
        )
        (getStudentById studentBody n)


getBondTarget : Bonds.Bond -> StudentBody -> Maybe Student.Student
getBondTarget bond ((StudentBody students _) as studentBody) =
    getStudentById studentBody (Bonds.getBondId bond)



--addBond : Int -> Int -> Student.Bond -> StudentBody -> Generator StudentBody
