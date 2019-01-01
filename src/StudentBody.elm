module StudentBody exposing
    ( StudentBody
    , addRelative
    , addStudent
    , addStudents
    , applyResults
    , asList
    , empty
    , findAllInteractions
    , findInteractions
    , fromList
    , genStudent
    , genWeeksInteractions
    , getBondTarget
    , getNum
    , getStudentById
    , processWeeksInteractions
    , updateStudentById
    )

import Array
import Bonds
import Classes
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


fromList : List Student.Student -> StudentBody
fromList list =
    StudentBody list (List.length list)


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


findInteractions : Student.Student -> StudentBody -> List Interactions.Interaction
findInteractions student body =
    let
        interactionsFromBond source b2 =
            Interactions.fromBond (interactionInfoGetter b2) (Student.getNumber source)
    in
    Student.getBonds student
        |> List.map (interactionsFromBond student body)
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


applyResults : StudentBody -> List Interactions.Result -> StudentBody
applyResults originalBody results =
    results
        |> List.concatMap Interactions.resultImpliesResults
        |> Debug.log "RESULTS TO APPLY"
        |> List.foldr
            (\result newBody ->
                let
                    simpleUpdate func sId =
                        updateStudentById func newBody sId
                            |> Maybe.withDefault newBody
                in
                case result of
                    Interactions.BondsFormed source bonds ->
                        bonds
                            |> List.foldr
                                (\bond acc2 ->
                                    Maybe.withDefault acc2 (addBondToStudentById bond source acc2)
                                )
                                newBody

                    Interactions.BondsBroken source bonds ->
                        bonds
                            |> List.foldr
                                (\bond acc2 ->
                                    Maybe.withDefault acc2 (breakBondToStudentById bond source acc2)
                                )
                                newBody

                    Interactions.NameChange sId name ->
                        simpleUpdate (\s -> Student.setName s name) sId

                    Interactions.PronounChange sId prons ->
                        simpleUpdate (\s -> Student.setPronouns s prons) sId

                    Interactions.StatsChange sId delta ->
                        simpleUpdate (\s -> Student.updateStats s delta) sId

                    Interactions.InvisStatsChange sId delta ->
                        simpleUpdate (\s -> Student.updateInvisibleStats s delta) sId
            )
            originalBody


interactionInfoGetter : StudentBody -> (Int -> Maybe Interactions.StudentInfo)
interactionInfoGetter body =
    \id ->
        getStudentById body id
            |> Maybe.map Student.getInteractionInfo


genWeeksEvents : StudentBody -> List Interactions.Interaction -> Generator (List Interactions.Event)
genWeeksEvents body ints =
    let
        infoGetter =
            interactionInfoGetter body
    in
    Interactions.interactionsToEvents infoGetter ints
        |> Random.andThen
            (\events ->
                asList body
                    |> List.map Student.getNumber
                    |> List.map (Interactions.genRandomEvent infoGetter)
                    |> Util.flattenGen
                    |> Random.map Util.removeNothings
                    |> Random.map (List.append events)
            )


resolveWeeksEvents : StudentBody -> List Interactions.Event -> StudentBody
resolveWeeksEvents body events =
    applyResults body (List.concatMap Interactions.resultsFromEvent events)


processWeeksInteractions : StudentBody -> List Interactions.Interaction -> Generator ( List Interactions.Event, StudentBody )
processWeeksInteractions body =
    genWeeksEvents body
        >> Random.map
            (\events ->
                ( events, resolveWeeksEvents body events )
            )


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
            interactionInfoGetter body
    in
    let
        classInteractions =
            possibleInteractions |> List.filter isClassInt

        otherInteractions =
            possibleInteractions |> List.filter (\x -> not <| isClassInt x) |> List.map (\x -> ( Interactions.getChance infoGetter x, x ))
    in
    Util.popNRandom 10 otherInteractions
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
                            ( bH, bR ) =
                                Bonds.randomNonRelative (Student.getNumber b)
                        in
                        Random.weighted bH bR

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


breakBondToStudentById : Bonds.Bond -> Int -> StudentBody -> Maybe StudentBody
breakBondToStudentById bond id body =
    let
        targetId =
            Bonds.getBondId bond

        sourceStudentM =
            getStudentById body id

        targetStudentM =
            getStudentById body (Bonds.getBondId bond)
    in
    Maybe.andThen identity <|
        Maybe.map2
            (\sourceStudent targetStudent ->
                let
                    sourceBonds =
                        Student.getBonds sourceStudent
                in
                case List.member bond sourceBonds of
                    False ->
                        Nothing

                    True ->
                        let
                            comm =
                                Bonds.isCommutative bond

                            ass =
                                Bonds.isAssociative bond

                            target =
                                Bonds.getBondId bond

                            updatedBody =
                                updateStudentById (Student.updateBonds (sourceBonds |> List.filter ((/=) bond))) body id
                        in
                        if not comm then
                            updatedBody

                        else
                            -- TODO implement the assoc case
                            updatedBody
                                |> Maybe.map
                                    (\usb2 ->
                                        case breakBondToStudentById (Bonds.changeBondId id bond) target usb2 of
                                            Just y ->
                                                y

                                            Nothing ->
                                                usb2
                                    )
            )
            sourceStudentM
            targetStudentM


addBondToStudentById : Bonds.Bond -> Int -> StudentBody -> Maybe StudentBody
addBondToStudentById bond id body =
    let
        targetId =
            Bonds.getBondId bond

        sourceStudentM =
            getStudentById body id

        targetStudentM =
            getStudentById body (Bonds.getBondId bond)
    in
    Maybe.andThen identity <|
        Maybe.map2
            (\sourceStudent targetStudent ->
                case Bonds.checkBondIsValidToAdd (Student.getBonds sourceStudent) (Student.getBonds targetStudent) bond id of
                    False ->
                        Nothing

                    True ->
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

                                                True ->
                                                    getStudentById suBody target
                                                        |> Maybe.map Student.getBonds
                                                        |> Maybe.map
                                                            (List.filter (Bonds.isOfSameType bond))
                                                        |> Maybe.map
                                                            (List.foldr
                                                                (\val acc ->
                                                                    case
                                                                        addBondToStudentById (Bonds.changeBondId id bond)
                                                                            (Bonds.getBondId val)
                                                                            acc
                                                                    of
                                                                        Just x ->
                                                                            x

                                                                        Nothing ->
                                                                            acc
                                                                )
                                                                suBody
                                                            )
                                        )
            )
            sourceStudentM
            targetStudentM


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
