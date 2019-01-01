module Student exposing
    ( Student
    , addBond
    , fromRecord
    , getBonds
    , getClasses
    , getFamilyName
    , getFriendlyName
    , getGivenName
    , getInteractionInfo
    , getInvisibleStats
    , getName
    , getNickname
    , getNumber
    , getPronouns
    , getQuirks
    , getStats
    , getWeapon
    , newRelative
    , newStudent
    , setName
    , setNickname
    , setNumber
    , setPronouns
    , updateBonds
    , updateInvisibleStats
    , updateStats
    )

import Array
import Basics exposing (round)
import Bonds
import Classes
import Dict exposing (Dict)
import Interactions
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
        , nickname : Maybe String
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
    case std.nickname of
        Just nick ->
            Name.toString std.name ++ " (" ++ nick ++ ")"

        Nothing ->
            Name.toString std.name


getFamilyName : Student -> String
getFamilyName (Student std) =
    Name.toFamilyString std.name


setName : Student -> Name.Name -> Student
setName (Student std) name =
    Student { std | name = name }


getNickname : Student -> Maybe String
getNickname (Student std) =
    std.nickname


setNickname : Student -> Maybe String -> Student
setNickname (Student std) nick =
    Student { std | nickname = nick }


getFriendlyName : Student -> String
getFriendlyName ((Student std) as s) =
    case std.nickname of
        Just nick ->
            nick

        Nothing ->
            getGivenName s


getGivenName : Student -> String
getGivenName (Student std) =
    Name.toGivenString std.name


getStats : Student -> VisibleStats
getStats (Student std) =
    std.visible


updateStats : Student -> VisibleStats -> Student
updateStats (Student s) d =
    Student { s | visible = Stats.relativeStatsUpdate s.visible d }


updateInvisibleStats : Student -> InvisibleStats -> Student
updateInvisibleStats (Student s) d =
    Student { s | invisible = Stats.relativeInvisStatsUpdate s.invisible d }


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
    Pronouns.asRecord pronouns


setPronouns : Student -> Pronouns.Pronoun -> Student
setPronouns (Student p) prons =
    Student { p | pronouns = prons }


getWeapon : Student -> WeaponType
getWeapon (Student { weapontype }) =
    weapontype


addBond : Bonds.Bond -> Student -> Student
addBond bond student =
    case student of
        Student ({ bonds } as std) ->
            Student { std | bonds = Bonds.add bond bonds }


updateBonds : List Bonds.Bond -> Student -> Student
updateBonds newBonds student =
    case student of
        Student ({ bonds } as std) ->
            Student { std | bonds = Bonds.fromList newBonds }


getBonds : Student -> List Bonds.Bond
getBonds student =
    case student of
        Student { bonds } ->
            Bonds.asList bonds


getInteractionInfo : Student -> Interactions.StudentInfo
getInteractionInfo ((Student s) as student) =
    { id = getNumber student
    , bonds = getBonds student
    , quirks = getQuirks student
    , name = s.name
    , pronouns = s.pronouns
    , visible = s.visible
    , invisible = s.invisible
    }


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


fromRecord sinfo =
    Student sinfo


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
                , nickname = Nothing
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
