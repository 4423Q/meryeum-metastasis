module Student exposing (Student, getFamilyName, getGivenName, getName, getNumber, getPronouns, getStats, getWeapon, newStudent)

import Array
import Basics exposing (round)
import Dict exposing (Dict)
import Name
import Random exposing (Generator)
import Random.Float
import Weapon exposing (WeaponType, newWepType, weapons)



-- They/Them/Their


type Pronoun
    = Pronoun String String String


pronounsList =
    [ ( 30, Pronoun "they" "them" "their" )
    , ( 30, Pronoun "he" "him" "his" )
    , ( 30, Pronoun "she" "her" "hers" )
    , ( 10, Pronoun "ze" "zir" "zirs" )
    , ( 10, Pronoun "ze" "hir" "hirs" )
    ]


newProns : Generator Pronoun
newProns =
    Random.weighted
        (case List.head pronounsList of
            Just x ->
                x

            Nothing ->
                ( 30, Pronoun "they" "them" "their" )
        )
        (case List.tail pronounsList of
            Just x ->
                x

            Nothing ->
                []
        )


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
    , danger : Stat
    , hot : Stat
    , extra : Stat
    }


newVisibleStats =
    Random.map4
        (\a b c d -> { sharp = a, danger = b, hot = c, extra = d })
        newStat
        newStat
        newStat
        newStat


type alias InvisibleStats =
    { horny : Stat
    , angry : Stat
    , ambition : Stat
    , diversion : Stat
    }


newInvisibleStats =
    Random.map4
        (\a b c d -> { horny = a, angry = b, ambition = c, diversion = d })
        newStat
        newStat
        newStat
        newStat


type Student
    = Student
        { num : Int
        , name : Name.Name
        , pronouns : Pronoun
        , weapontype : WeaponType
        , visible : VisibleStats
        , invisible : InvisibleStats
        , bonds : Dict String Bond
        , value : Int
        }


getName : Student -> String
getName (Student std) =
    Name.toString std.name


getFamilyName : Student -> String
getFamilyName (Student std) =
    Name.toFamilyString std.name


getGivenName : Student -> String
getGivenName (Student std) =
    Name.toGivenString std.name


getStats : Student -> VisibleStats
getStats (Student std) =
    std.visible


getNumber : Student -> Int
getNumber (Student std) =
    std.num


getPronouns : Student -> { subj : String, obj : String, pos : String }
getPronouns (Student { pronouns }) =
    case pronouns of
        Pronoun a b c ->
            { subj = a, obj = b, pos = c }


getWeapon : Student -> WeaponType
getWeapon (Student { weapontype }) =
    weapontype


newStudent : Int -> Generator Student
newStudent studentNumber =
    Random.map5
        (\vs is name pro wep ->
            Student
                { num = studentNumber
                , name = name
                , pronouns = pro
                , weapontype = wep
                , visible = vs
                , invisible = is
                , bonds = Dict.empty
                , value = 0
                }
        )
        newVisibleStats
        newInvisibleStats
        Name.newName
        newProns
        newWepType
