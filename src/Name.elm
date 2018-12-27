module Name exposing (Name, newName, toString)

import Array
import Basics exposing (round)
import List.Extra
import Random exposing (Generator)
import Random.Float


names =
    Array.fromList
        [ "Addax"
        , "Aegis"
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
        , "Cherise"
        , "Clarity"
        , "Cobra"
        , "Cordova"
        , "Cruz"
        , "Dawn"
        , "Dragoon"
        , "Endings"
        , "Enoshima"
        , "Etienne"
        , "Fedor"
        , "Gary"
        , "Gilbres"
        , "Grace"
        , "Henk"
        , "Honk"
        , "Ismael"
        , "Jennifer"
        , "Joven"
        , "Karen"
        , "Keith"
        , "Kilbride"
        , "Kingdom"
        , "Kitty"
        , "Lamar"
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
        , "Nat"
        , "Palace"
        , "Peel"
        , "Pelenor"
        , "Perrin"
        , "Plue"
        , "Rabia"
        , "Wasim"
        , "Rashid"
        , "Claire"
        , "Basel"
        , "Sachiko"
        , "Priest"
        , "Q"
        , "Reed"
        , "Replica"
        , "Rose"
        , "Ruby"
        , "Rye"
        , "Sanctity"
        , "Schnee"
        , "Seitur"
        , "Shin"
        , "Erin"
        , "Sonon"
        , "Temerity"
        , "Theodor"
        , "Two"
        , "Ultima"
        , "Untimely"
        , "Weiss"
        , "Asano"
        , "X"
        , "Daryl"
        , "Kiba"
        , "Careless"
        , "Sora"
        , "Kaz"
        , "Kazmir"
        , "Sorrel"
        , "Foon"
        , "Kwame"
        , "Iapetus"
        , "Tethys"
        , "Char"
        , "Pechen"
        , "Quan"
        , "Daisy"
        , "Xi"
        , "Xiao"
        , "Yang"
        , "Yubi"
        , "k"
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
