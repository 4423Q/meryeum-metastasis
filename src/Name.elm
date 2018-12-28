module Name exposing (Name, newName, toFamilyString, toGivenString, toString)

import Array
import Basics exposing (round)
import List.Extra
import Random exposing (Generator)
import Random.Float


namesArray =
    Array.fromList
        [ "Aadhya"
        , "Aarav"
        , "Addax"
        , "Aegis"
        , "Algernon"
        , "Alpha"
        , "Alvarez"
        , "Amuro"
        , "Playa"
        , "Pistil"
        , "Chrome"
        , "April"
        , "Art"
        , "Asano"
        , "August"
        , "Basel"
        , "Belladonna"
        , "Berg"
        , "Blake"
        , "Borges"
        , "Bridge"
        , "Bravo"
        , "Brink"
        , "Calamity"
        , "Calibre"
        , "Careless"
        , "Carta"
        , "Celia"
        , "Ceres"
        , "Chanbara"
        , "Chandra"
        , "Char"
        , "Chasm"
        , "Cherise"
        , "Claire"
        , "Clarity"
        , "Cobra"
        , "Cordova"
        , "Cruz"
        , "Daisy"
        , "Daryl"
        , "Dawn"
        , "December"
        , "Diya"
        , "Dragoon"
        , "Eager"
        , "Endings"
        , "Enoshima"
        , "Erin"
        , "Etienne"
        , "February"
        , "Fedor"
        , "Foon"
        , "Gary"
        , "Gilbres"
        , "Giren"
        , "Grace"
        , "Hasma"
        , "Henk"
        , "Hime"
        , "Honk"
        , "Seisma"
        , "Iapetus"
        , "Ismael"
        , "January"
        , "Jennifer"
        , "Jorge"
        , "Joven"
        , "July"
        , "June"
        , "Karen"
        , "Kaz"
        , "Kazmir"
        , "Keble"
        , "Keith"
        , "Kiba"
        , "Kilbride"
        , "Kingdom"
        , "Kitty"
        , "Kuro"
        , "Kim"
        , "Kwame"
        , "Lamar"
        , "Laplace"
        , "Links"
        , "Long"
        , "Mako"
        , "Mal"
        , "March"
        , "Marida"
        , "Maryam"
        , "May"
        , "San"
        , "Galen"
        , "Mira"
        , "Ong"
        , "Medea"
        , "Meryuem"
        , "Metastasis"
        , "Mishka"
        , "Amar"
        , "Pewter"
        , "Void"
        , "EX//HALE"
        , "Remembrance"
        , "Toady"
        , "Reppa"
        , "Pliskin"
        , "Theta"
        , "Sigma"
        , "Uratora"
        , "Cosmos"
        , "Momo"
        , "Mu"
        , "Mulder"
        , "Nadiya"
        , "Nakamura"
        , "Nat"
        , "Nine"
        , "November"
        , "Nyanna"
        , "Olive"
        , "Palace"
        , "Pechen"
        , "Peel"
        , "Pelenor"
        , "Perrin"
        , "Plue"
        , "Pool"
        , "Priest"
        , "Q"
        , "Quan"
        , "Rabia"
        , "Rainy"
        , "Rashid"
        , "Reed"
        , "Ren"
        , "Replica"
        , "Rose"
        , "Ruby"
        , "Rye"
        , "Sachiko"
        , "Saif"
        , "Blue"
        , "Crimson"
        , "Azure"
        , "Scarlet"
        , "Sanctity"
        , "Schnee"
        , "Seitur"
        , "September"
        , "Seven"
        , "Shanaya"
        , "Shin"
        , "Siya"
        , "Solace"
        , "Sonon"
        , "Sora"
        , "Sorrel"
        , "Sunny"
        , "Temerity"
        , "Tethys"
        , "Thalia"
        , "Theodor"
        , "Twain"
        , "Twelve"
        , "Two"
        , "Ultima"
        , "Untimely"
        , "Villalobos"
        , "Warren"
        , "Wasim"
        , "Watari"
        , "Weiss"
        , "X"
        , "Xi"
        , "Xiao"
        , "Zhou"
        , "Yang"
        , "Yubi"
        , "k"
        ]


getNameFromList : Generator String
getNameFromList =
    Random.map
        (\n ->
            case Array.get n namesArray of
                Just x ->
                    x

                Nothing ->
                    "ERROR"
        )
        (Random.int 0 (Array.length namesArray - 1))


type Name
    = Name
        { names : List String
        , separators : List Connector
        , family : Int
        , given : Int
        }


coalesce : List String -> List Connector -> ( List String, List Connector )
coalesce names separators =
    case List.length names of
        0 ->
            ( [], [] )

        _ ->
            case names of
                topName :: bottomNames ->
                    case separators of
                        topSep :: bottomSeps ->
                            let
                                ( newNames, newSeps ) =
                                    coalesce bottomNames bottomSeps
                            in
                            case topSep of
                                Breaking y ->
                                    case newNames of
                                        z :: zs ->
                                            ( topName :: String.trim (y ++ z) :: zs, bottomSeps )

                                        _ ->
                                            ( topName :: newNames, bottomSeps )

                                NonBreaking y ->
                                    case newNames of
                                        z :: zs ->
                                            ( (topName ++ y ++ z) :: zs, bottomSeps )

                                        zs ->
                                            ( topName :: zs, bottomSeps )

                        _ ->
                            ( names, [] )

                _ ->
                    ( [], [] )


toString : Name -> String
toString name =
    case name of
        Name { names, separators, family } ->
            String.join "" <| List.Extra.interweave names (List.map getSepString separators)


getNameAndSeparatorString : List String -> List Connector -> Int -> String
getNameAndSeparatorString names seps position =
    let
        baseName =
            case List.drop position names |> List.head of
                Just x ->
                    x

                Nothing ->
                    "ERROR"
    in
    case position of
        0 ->
            baseName

        _ ->
            let
                lastsep =
                    List.drop (position - 1) seps |> List.head
            in
            case lastsep of
                Just (Breaking x) ->
                    String.trim (x ++ baseName)

                Just (NonBreaking x) ->
                    getNameAndSeparatorString names seps (position - 1) ++ x ++ baseName

                Nothing ->
                    "ERROR in case of getting last separator! " ++ String.fromInt position


toFamilyString : Name -> String
toFamilyString name =
    case name of
        Name { names, separators, family } ->
            getNameAndSeparatorString names separators family


toGivenString : Name -> String
toGivenString name =
    case name of
        Name { names, separators, given } ->
            case List.drop given names |> List.head of
                Just x ->
                    x

                Nothing ->
                    "ERROR Getting given name"


newConnector : Generator Connector
newConnector =
    Random.weighted
        ( 85, Breaking " " )
        [ ( 10, NonBreaking "-" )
        , ( 5, NonBreaking "'" )
        , ( 5, Breaking " d'" )
        , ( 5, Breaking " la " )
        , ( 5, NonBreaking " of " )
        , ( 5, Breaking " of " )
        , ( 5, Breaking " v'" )
        , ( 5, NonBreaking " v'" )
        , ( 2, Breaking " whist-" )
        , ( 1, NonBreaking " the " )
        ]


type Connector
    = Breaking String
    | NonBreaking String


getSepString con =
    case con of
        Breaking x ->
            x

        NonBreaking x ->
            x


newName : Generator Name
newName =
    Random.map
        (\n ->
            if n < 1 then
                1

            else
                round n
        )
        (Random.Float.normal 2.3 0.6)
        |> Random.andThen (\len -> Random.list len getNameFromList)
        |> Random.andThen
            (\xs ->
                Random.map2
                    (\cons len -> ( xs, cons, len ))
                    (Random.list (List.length xs - 1) newConnector)
                    (Random.int 0 (List.length xs - 1))
            )
        |> Random.andThen
            (\( xs, cons, fam ) ->
                Random.map
                    (\given -> { names = xs, separators = cons, family = fam, given = given })
                    (case List.length xs of
                        1 ->
                            Random.constant 0

                        _ ->
                            let
                                firstBitLen =
                                    fam

                                lastBitLen =
                                    List.length xs - (fam + 1)
                            in
                            Random.andThen identity <|
                                Random.weighted
                                    ( toFloat firstBitLen, Random.int 0 (firstBitLen - 1) )
                                    [ ( toFloat lastBitLen, Random.int (fam + 1) (fam + lastBitLen) ) ]
                    )
            )
        |> Random.map
            (\x ->
                Name x
            )
