module Classes exposing (Class(..), genRandomClass, toString)

import Quirks
import Random exposing (Generator)
import Weapon exposing (WeaponType)


type Class
    = History
    | Mathematics
    | Physics
    | Philosophy
    | Chemistry
    | Biology
    | Art
    | Warcraft
    | Piloting
    | SelfActualisation
    | AuraControl
    | Combat
    | Literature
    | CreativeWriting
    | ThoughtControl
    | BecomingAGod


availableRandomClasses =
    [ History
    , Mathematics
    , Physics
    , Philosophy
    , Chemistry
    , Biology
    , Art
    , Warcraft
    , Piloting
    , SelfActualisation
    , AuraControl
    , Combat
    , Literature
    , CreativeWriting
    ]


toString : Class -> String
toString class =
    case class of
        History ->
            "History"

        Mathematics ->
            "Mathematics"

        Physics ->
            "Physics"

        Philosophy ->
            "Philosophy"

        Chemistry ->
            "Chemistry"

        Biology ->
            "Biology"

        Art ->
            "Art"

        Warcraft ->
            "The Art Of War"

        Piloting ->
            "Piloting"

        SelfActualisation ->
            "Self Actualisation"

        AuraControl ->
            "Aura Control"

        Combat ->
            "Combat"

        Literature ->
            "Literature"

        CreativeWriting ->
            "Creative Writing"

        ThoughtControl ->
            "Thought Control"

        BecomingAGod ->
            "Becoming a God"


type alias ClassInput =
    { weapon : WeaponType
    , quirks : List Quirks.Quirk
    , classes : List Class
    }


genRandomClass : ClassInput -> Generator Class
genRandomClass input =
    let
        chanceList =
            availableRandomClasses
                |> List.map (\x -> ( getClassChance input x, x ))
    in
    case chanceList of
        x :: xs ->
            Random.weighted x xs

        [] ->
            Random.constant History


getClassChance : ClassInput -> Class -> Float
getClassChance { weapon, quirks, classes } class =
    if List.member class classes then
        0

    else
        case class of
            BecomingAGod ->
                if List.member (Quirks.Quirk Quirks.DestinedForGreatness) quirks then
                    1

                else
                    0

            ThoughtControl ->
                if List.member (Quirks.Quirk Quirks.Illuminati) quirks then
                    50

                else
                    0

            Warcraft ->
                case weapon of
                    Weapon.Commander ->
                        50

                    Weapon.Tactician ->
                        50

                    _ ->
                        1

            Piloting ->
                case weapon of
                    Weapon.Pilot _ ->
                        50

                    _ ->
                        1

            SelfActualisation ->
                case weapon of
                    Weapon.Weapon _ ->
                        20

                    _ ->
                        1

            AuraControl ->
                case weapon of
                    Weapon.Soldier ->
                        5

                    _ ->
                        0.2

            Combat ->
                if List.member (Quirks.Quirk Quirks.LovesToFight) quirks then
                    50

                else
                    1

            CreativeWriting ->
                if List.member (Quirks.Quirk Quirks.Fanfic) quirks then
                    50

                else
                    1

            _ ->
                1
