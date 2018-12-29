module Student exposing
    ( Student
    , addBond
    , getBonds
    , getClasses
    , getFamilyName
    , getGivenName
    , getInvisibleStats
    , getName
    , getNumber
    , getPronouns
    , getQuirks
    , getStats
    , getWeapon
    , newRelative
    , newStudent
    , setNumber
    )

import Array
import Basics exposing (round)
import Bonds
import Classes
import Dict exposing (Dict)
import Name
import Pronouns exposing (Pronoun)
import Quirks
import Random exposing (Generator)
import Random.Float
import Stats exposing (InvisibleStats, Stat, VisibleStats)
import Weapon exposing (WeaponType, newWepType, weapons)


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
        , quirks : List Quirks.Quirk
        , classes : List Classes.Class
        }


genAndTakeClasses : Int -> Student -> Generator Student
genAndTakeClasses n student =
    case n of
        0 ->
            Random.constant student

        _ ->
            Classes.genRandomClass
                { weapon = getWeapon student
                , quirks = getQuirks student
                , classes = getClasses student
                }
                |> Random.andThen
                    (\class ->
                        case addClass student class of
                            Just student2 ->
                                genAndTakeClasses (n - 1) student2

                            Nothing ->
                                genAndTakeClasses n student
                    )


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


getInvisibleStats : Student -> InvisibleStats
getInvisibleStats (Student s) =
    s.invisible


getNumber : Student -> Int
getNumber (Student std) =
    std.num


setNumber : Int -> Student -> Student
setNumber num student =
    case student of
        Student x ->
            Student { x | num = num }


getPronouns : Student -> { subj : String, obj : String, pos : String, pos2 : String }
getPronouns (Student { pronouns }) =
    case pronouns of
        Pronouns.Pronoun a b c d ->
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


addQuirk : Student -> Quirks.Quirk -> Student
addQuirk (Student s) quirk =
    if List.member quirk s.quirks then
        Student s

    else
        Student { s | quirks = quirk :: s.quirks }


addClass : Student -> Classes.Class -> Maybe Student
addClass (Student s) class =
    if List.member class s.classes then
        Nothing

    else
        Just (Student { s | classes = class :: s.classes })


getQuirks : Student -> List Quirks.Quirk
getQuirks (Student s) =
    s.quirks


getClasses : Student -> List Classes.Class
getClasses (Student s) =
    s.classes


newStudentMaker : Generator Name.Name -> Generator Student
newStudentMaker nameGen =
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
                , classes = []
                , quirks = []
                , value = 0
                }
        )
        Stats.newVisibleStats
        Stats.newInvisibleStats
        nameGen
        Pronouns.newProns
        newWepType
        |> Random.andThen
            (\((Student s) as student) ->
                Quirks.genRandomQuirk { visibleStats = s.visible, invisibleStats = s.invisible, weapontype = s.weapontype }
                    |> Random.map
                        (\q ->
                            case q of
                                Just qu ->
                                    addQuirk student qu

                                Nothing ->
                                    student
                        )
            )
        |> Random.andThen
            (\student ->
                Random.float 4 (4 + ((getStats student).sharp |> toFloat) / 2.5)
                    |> Random.andThen
                        (\classnum ->
                            genAndTakeClasses (Basics.round classnum) student
                        )
            )


newStudent : Generator Student
newStudent =
    newStudentMaker Name.newName


newRelative : Student -> Generator Student
newRelative (Student { name }) =
    newStudentMaker (Name.newRelativeName name)
