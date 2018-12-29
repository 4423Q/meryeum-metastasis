module Quirks exposing (Quirk(..), QuirkName(..), availableRandomQuirks)

import Random exposing (Generator)


type QuirkName
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


type Quirk
    = Quirk QuirkName


availableRandomQuirks =
    [ Quirk HasADog
    , Quirk LovesToFight
    , Quirk StoleAMecha
    , Quirk DestinedForGreatness
    , Quirk LovesHotDogs
    , Quirk KeepsABulletJournal
    , Quirk FightsBlindfolded
    , Quirk ExtraHot
    , Quirk RulesNerd
    , Quirk Fanfic
    , Quirk LiveStreamsTraining
    , Quirk TeamCaptain
    , Quirk Homesick
    ]
