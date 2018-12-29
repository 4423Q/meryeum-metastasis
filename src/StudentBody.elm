module StudentBody exposing
    ( StudentBody
    , addRelative
    , addStudent
    , addStudents
    , asList
    , empty
    , genStudent
    , getBondTarget
    , getNum
    , getStudentById
    )

import Array
import Bonds
import Random exposing (Generator)
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
            ( 90, Random.map (\x -> Just (addStudent x studentBody)) newStudent )
            [ ( 10, genRandomRelative studentBody ) ]


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
                                addStudents (n - 1) studentBody
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
    Random.map2
        (\student bond ->
            let
                ( addedStudent, newBody ) =
                    addStudent student body
            in
            addBondToStudentById bond (Student.getNumber addedStudent) newBody
                |> Maybe.map
                    (\body2 -> ( addedStudent, body2 ))
        )
        (Student.newRelative target)
        (Random.uniform (Bonds.Relative Bonds.Sibling targetNumber)
            [ Bonds.Relative Bonds.Cousin targetNumber ]
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
