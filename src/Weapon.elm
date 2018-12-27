module Weapon exposing (WeaponType(..), newWepType, weapons)

import Array
import Random exposing (Generator)
import Util


weapons =
    Array.fromList
        [ "Scythe"
        , "Revolver"
        , "Sniper Rifle"
        , "Machine Gun"
        , "Sword"
        , "Spear"
        , "Mace"
        , "Axe"
        , "Polearm"
        , "Orbital Cannon"
        , "Knife"
        , "Chakram"
        , "Pair of Brass Knuckles"
        , "Regular Everyday Kind of Person Don't Even Worry About It"
        ]


pilotables =
    Array.fromList
        [ "Mecha"
        , "Fighter"
        , "Battleship"
        , "Planet-class Star Cruiser"
        ]


type WeaponType
    = Weapon String
    | Pilot String
    | Soldier


newWepType : Generator WeaponType
newWepType =
    Random.andThen identity <|
        Random.uniform
            (Util.genUniformFromArray
                Weapon
                "ERROR"
                weapons
            )
            [ Random.constant Soldier
            , Util.genUniformFromArray
                Pilot
                "ERROR"
                pilotables
            ]
