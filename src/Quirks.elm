module Quirks exposing (Quirk(..), availableRandomQuirks, genRandomQuirk, getQuirkChance)

import Random exposing (Generator)
import Stats
import Weapon


type Quirk
    = HasADog
    | LovesToFight
    | StoleAMecha
    | DestinedForGreatness
    | LovesHotDogs
    | KeepsABulletJournal
    | FightsBlindfolded
    | ExtraHot
    | RulesNerd
    | Fanfic
    | LiveStreamsTraining
    | TeamCaptain
    | Homesick
    | Illuminati
    | CatEars
    | WolfEars


availableRandomQuirks =
    [ HasADog
    , LovesToFight
    , StoleAMecha
    , DestinedForGreatness
    , LovesHotDogs
    , KeepsABulletJournal
    , FightsBlindfolded
    , ExtraHot
    , RulesNerd
    , Fanfic
    , LiveStreamsTraining
    , TeamCaptain
    , Homesick
    , CatEars
    , WolfEars
    ]


type alias ClassInput =
    { visibleStats : Stats.VisibleStats
    , invisibleStats : Stats.InvisibleStats
    , weapontype : Weapon.WeaponType
    }


getQuirkChance : ClassInput -> Quirk -> Float
getQuirkChance { visibleStats, invisibleStats, weapontype } name =
    let
        vS =
            visibleStats

        iS =
            invisibleStats

        wep =
            weapontype
    in
    case name of
        Illuminati ->
            1

        HasADog ->
            1

        LovesHotDogs ->
            1

        Homesick ->
            1

        CatEars ->
            1

        WolfEars ->
            1

        KeepsABulletJournal ->
            if vS.sharp > 6 then
                5

            else
                1

        LovesToFight ->
            if iS.angry >= 8 && vS.danger >= 7 then
                100

            else
                0

        RulesNerd ->
            case wep of
                Weapon.Tactician ->
                    5

                Weapon.Commander ->
                    5

                _ ->
                    0

        StoleAMecha ->
            case wep of
                Weapon.Pilot "Mecha" ->
                    toFloat iS.ambition * 10 + toFloat vS.danger * 5

                _ ->
                    0

        DestinedForGreatness ->
            if vS.extra >= 8 && iS.ambition >= 8 then
                10

            else
                0

        ExtraHot ->
            if vS.hot >= 10 then
                10

            else
                0

        Fanfic ->
            if vS.sharp >= 7 && iS.horny >= 7 then
                15

            else
                1

        LiveStreamsTraining ->
            if vS.extra >= 8 && iS.diversion >= 6 then
                10

            else
                0

        TeamCaptain ->
            if vS.danger >= 8 && iS.diversion >= 8 then
                5

            else
                0

        FightsBlindfolded ->
            case wep of
                Weapon.Soldier ->
                    if vS.extra > 8 then
                        20

                    else
                        0

                _ ->
                    0


genRandomQuirk : ClassInput -> Generator (Maybe Quirk)
genRandomQuirk input =
    availableRandomQuirks
        |> List.map
            (\x -> ( getQuirkChance input x, Just x ))
        |> (\y ->
                case y of
                    [] ->
                        Random.weighted ( 100, Nothing ) []

                    zs ->
                        Random.weighted ( 100, Nothing ) zs
           )
