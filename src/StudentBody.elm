module StudentBody exposing (StudentBody, addStudent, addStudents, asList, empty, getNum, getStudentById)

import Random exposing (Generator)
import Student exposing (newStudent)



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


addStudent : StudentBody -> Generator StudentBody
addStudent ((StudentBody students lastNum) as studentBody) =
    Random.map
        (\student -> StudentBody (student :: students) (lastNum + 1))
        (newStudent lastNum)


addStudents : Int -> StudentBody -> Generator StudentBody
addStudents n studentBody =
    case n of
        0 ->
            Random.constant studentBody

        _ ->
            Random.andThen identity <|
                Random.map
                    (\newBody -> addStudents (n - 1) newBody)
                    (addStudent studentBody)
