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
        , "Longsword"
        , "Spear"
        , "Mace"
        , "Axe"
        , "Polearm"
        , "Orbital Cannon"
        , "Knife"
        , "Chakram"
        , "Katar"
        , "Iron Fan"
        , "Cutlass"
        , "Sabre"
        , "Halberd"
        , "Greatsword"
        , "Sickle"
        , "Baton"
        , "Lance"
        , "Trident"
        , "Pike"
        , "Voulge"
        , "Longbow"
        , "Crossbow"
        , "Arbalest"
        , "Chain Whip"
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
    | Commander
    | Tactician
    | Special


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
            , Random.constant Commander
            , Random.constant Tactician
            , Util.genUniformFromArray
                Pilot
                "ERROR"
                pilotables
            ]
