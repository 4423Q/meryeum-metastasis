module Name exposing (Name, newName, toString)

import Array
import Basics exposing (round)
import List.Extra
import Random exposing (Generator)
import Random.Float


names =
    Array.fromList
        [ "Addax"
        , "Algernon"
        , "Alpha"
        , "Alvarez"
        , "Amuro"
        , "Art"
        , "Belladonna"
        , "Blake"
        , "Bridge"
        , "Calamity"
        , "Carta"
        , "Celia"
        , "Chanbara"
        , "Chandra"
        , "Chasm"
        , "Clarity"
        , "Cobra"
        , "Cordova"
        , "Cruz"
        , "Dawn"
        , "Endings"
        , "Enoshima"
        , "Etienne"
        , "Fedor"
        , "Gilbres"
        , "Grace"
        , "Jennifer"
        , "Joven"
        , "Karen"
        , "Keith"
        , "Kilbride"
        , "Kingdom"
        , "Laplace"
        , "Links"
        , "Long"
        , "Mako"
        , "Mal"
        , "Marida"
        , "Meryuem"
        , "Mohammed"
        , "Nadiya"
        , "Nakamura"
        , "Palace"
        , "Peel"
        , "Pelenor"
        , "Plue"
        , "Priest"
        , "Reed"
        , "Replica"
        , "Rose"
        , "Ruby"
        , "Sanctity"
        , "Schnee"
        , "Seitur"
        , "Shin"
        , "Sonon"
        , "Temerity"
        , "Theodor"
        , "Two"
        , "Weiss"
        , "X"
        , "Xi"
        , "Xiao"
        , "Yang"
        , "k"
        , "Ismael"
        , "Ultima"
        , "Untimely"
        , "Lamar"
        , "Cherise"
        , "Dragoon"
        , "Henk"
        , "Honk"
        , "Aegis"
        , "Q"
        , "Rye"
        , "Yubi"
        , "Nat"
        , "Gary"
        , "Kitty"
        ]


getNameFromList : Generator String
getNameFromList =
    Random.map
        (\n ->
            case Array.get n names of
                Just x ->
                    x

                Nothing ->
                    "ERROR"
        )
        (Random.int 0 (Array.length names - 1))


type Connector
    = Space
    | Dash
    | Apos


type Name
    = Name String


toString : Name -> String
toString name =
    case name of
        Name str ->
            str


newConnector : Generator Connector
newConnector =
    Random.weighted
        ( 85, Space )
        [ ( 10, Dash ), ( 5, Apos ) ]


connectorToString : Connector -> String
connectorToString ctr =
    case ctr of
        Space ->
            " "

        Dash ->
            "-"

        Apos ->
            "'"


newName : Generator Name
newName =
    Random.map
        (\n ->
            if n < 1 then
                1

            else
                round n
        )
        (Random.Float.normal 2.2 0.6)
        |> Random.andThen (\len -> Random.list len getNameFromList)
        |> Random.andThen
            (\xs ->
                Random.map
                    (\cons -> List.Extra.interweave xs (List.map connectorToString cons))
                    (Random.list (List.length xs - 1) newConnector)
            )
        |> Random.map (\n -> Name (String.join "" n))
