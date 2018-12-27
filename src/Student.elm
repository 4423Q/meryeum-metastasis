module Student exposing (Student, StudentBody(..), getName, getNumber, getStats, newStudent)

import Basics exposing (round)
import Dict exposing (Dict)
import Name
import Random exposing (Generator)
import Random.Float


type WeaponType
    = Weapon String
    | Pilot String
    | Soldier String


type Bond
    = Friend


type alias Stat =
    Int


newStat : Generator Stat
newStat =
    Random.map
        (\n ->
            if n > 10 then
                10

            else if n < 0 then
                0

            else
                round n
        )
        (Random.Float.normal 5 2)


type alias VisibleStats =
    { sharp : Stat
    , fight : Stat
    , hot : Stat
    , extra : Stat
    }


newVisibleStats =
    Random.map4
        (\a b c d -> { sharp = a, fight = b, hot = c, extra = d })
        newStat
        newStat
        newStat
        newStat


type alias InvisibleStats =
    {}


type Student
    = Student
        { num : Int
        , name : Name.Name
        , weapontype : WeaponType
        , visible : VisibleStats
        , invisible : InvisibleStats
        , bonds : Dict String Bond
        , value : Int
        }


getName : Student -> String
getName (Student std) =
    Name.toString std.name


getStats : Student -> VisibleStats
getStats (Student std) =
    std.visible


getNumber : Student -> Int
getNumber (Student std) =
    std.num


newStudent : StudentBody -> Generator Student
newStudent (StudentBody _ lastNum) =
    Random.map2
        (\vs name ->
            Student
                { num = lastNum + 1
                , name = name
                , weapontype = Pilot "Dog"
                , visible = vs
                , invisible = {}
                , bonds = Dict.empty
                , value = 0
                }
        )
        newVisibleStats
        Name.newName



-- StudentBody has a list of students and the current largest student number


type StudentBody
    = StudentBody (List Student) Int
