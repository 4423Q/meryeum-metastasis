module Student exposing
    ( Student
    , addBond
    , getBonds
    , getFamilyName
    , getGivenName
    , getName
    , getNumber
    , getPronouns
    , getStats
    , getWeapon
    , newRelative
    , newStudent
    , setNumber
    )

import Array
import Basics exposing (round)
import Bonds
import Dict exposing (Dict)
import Name
import Random exposing (Generator)
import Random.Float
import Weapon exposing (WeaponType, newWepType, weapons)



-- They/Them/Their/Their - She/Her/Her/Hers


type Pronoun
    = Pronoun String String String String


pronounsList =
    [ ( 30, Pronoun "they" "them" "their" "their" )
    , ( 30, Pronoun "he" "him" "his" "his" )
    , ( 30, Pronoun "she" "her" "her " "hers" )
    , ( 10, Pronoun "ze" "zir" "zir" "zirs" )
    , ( 10, Pronoun "ze" "hir" "hir" "hirs" )
    , ( 10, Pronoun "xe" "xem" "xir" "xirs" )
    ]


newProns : Generator Pronoun
newProns =
    Random.weighted
        (case List.head pronounsList of
            Just x ->
                x

            Nothing ->
                ( 30, Pronoun "they" "them" "their" "their" )
        )
        (case List.tail pronounsList of
            Just x ->
                x

            Nothing ->
                []
        )


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
        , bonds : Bonds.Bonds
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


getPronouns : Student -> { subj : String, obj : String, pos : String, pos2 : String }
getPronouns (Student { pronouns }) =
    case pronouns of
        Pronoun a b c d ->
            { subj = a, obj = b, pos = c, pos2 = d }


getWeapon : Student -> WeaponType
getWeapon (Student { weapontype }) =
    weapontype


addBond : Bonds.Bond -> Student -> Student
addBond bond student =
    case student of
        Student ({ bonds } as std) ->
            Student { std | bonds = Bonds.add bond bonds }


getBonds : Student -> List Bonds.Bond
getBonds student =
    case student of
        Student { bonds } ->
            Bonds.asList bonds


newStudent : Generator Student
newStudent =
    Random.map5
        (\vs is name pro wep ->
            Student
                { num = 0
                , name = name
                , pronouns = pro
                , weapontype = wep
                , visible = vs
                , invisible = is
                , bonds = Bonds.empty
                , value = 0
                }
        )
        newVisibleStats
        newInvisibleStats
        Name.newName
        newProns
        newWepType


setNumber : Int -> Student -> Student
setNumber num student =
    case student of
        Student x ->
            Student { x | num = num }


newRelative : Student -> Generator Student
newRelative student =
    case student of
        Student { name } ->
            Random.map5
                (\vs is newname pro wep ->
                    Student
                        { num = 0
                        , name = newname
                        , pronouns = pro
                        , weapontype = wep
                        , visible = vs
                        , invisible = is
                        , bonds = Bonds.empty
                        , value = 0
                        }
                )
                newVisibleStats
                newInvisibleStats
                (Name.newRelativeName name)
                newProns
                newWepType
