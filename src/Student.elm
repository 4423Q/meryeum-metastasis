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
import Quirks
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
        , quirks : List Quirks.Quirk
        , classes : List Classes.Class
        }


getQuirkChance : Student -> Quirks.Quirk -> Float
getQuirkChance student (Quirks.Quirk name) =
    let
        vS =
            getStats student

        iS =
            getInvisibleStats student

        wep =
            getWeapon student
    in
    case name of
        Quirks.Illuminati ->
            1

        Quirks.HasADog ->
            1

        Quirks.LovesHotDogs ->
            1

        Quirks.Homesick ->
            1

        Quirks.KeepsABulletJournal ->
            if vS.sharp > 6 then
                5

            else
                1

        Quirks.LovesToFight ->
            if iS.angry >= 8 && vS.danger >= 7 then
                100

            else
                0

        Quirks.RulesNerd ->
            case wep of
                Weapon.Tactician ->
                    5

                Weapon.Commander ->
                    5

                _ ->
                    0

        Quirks.StoleAMecha ->
            case wep of
                Weapon.Pilot "Mecha" ->
                    toFloat iS.ambition * 20

                _ ->
                    0

        Quirks.DestinedForGreatness ->
            if vS.extra >= 8 && iS.ambition >= 8 then
                10

            else
                0

        Quirks.ExtraHot ->
            if vS.hot >= 10 then
                10

            else
                0

        Quirks.Fanfic ->
            if vS.sharp >= 7 && iS.horny >= 7 then
                15

            else
                1

        Quirks.LiveStreamsTraining ->
            if vS.extra >= 8 && iS.diversion >= 6 then
                10

            else
                0

        Quirks.TeamCaptain ->
            if vS.danger >= 8 && iS.diversion >= 8 then
                5

            else
                0

        Quirks.FightsBlindfolded ->
            case wep of
                Weapon.Soldier ->
                    if vS.extra > 8 then
                        20

                    else
                        0

                _ ->
                    0


genRandomQuirk : Student -> Generator (Maybe Quirks.Quirk)
genRandomQuirk student =
    Quirks.availableRandomQuirks
        |> List.map
            (\x -> ( getQuirkChance student x, Just x ))
        |> (\y ->
                case y of
                    [] ->
                        Random.weighted ( 100, Nothing ) []

                    zs ->
                        Random.weighted ( 100, Nothing ) zs
           )


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
        newVisibleStats
        newInvisibleStats
        nameGen
        newProns
        newWepType
        |> Random.andThen
            (\student ->
                genRandomQuirk student
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
                Random.int 4 6
                    |> Random.andThen
                        (\classnum ->
                            genAndTakeClasses classnum student
                        )
            )


newStudent =
    newStudentMaker Name.newName


newRelative : Student -> Generator Student
newRelative (Student { name }) =
    newStudentMaker (Name.newRelativeName name)


setNumber : Int -> Student -> Student
setNumber num student =
    case student of
        Student x ->
            Student { x | num = num }
