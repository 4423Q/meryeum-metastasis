module Name exposing (Name, newName, newRelativeName, toFamilyString, toGivenString, toString)

import Array
import Basics exposing (round)
import List.Extra
import Random exposing (Generator)
import Random.Float


namesArray =
    Array.fromList
        [ "00"
        , "Aadhya"
        , "Aaliyah"
        , "Aarav"
        , "Absolute"
        , "Addax"
        , "Aegis"
        , "Sappho"
        , "Aether"
        , "Silver"
        , "Ai"
        , "Algernon"
        , "Alpha"
        , "Altar"
        , "Alvarez"
        , "Amar"
        , "Amuro"
        , "Tharsis"
        , "Terra"
        , "Apex"
        , "April"
        , "Art"
        , "Arturia"
        , "Asano"
        , "August"
        , "Autumn"
        , "Azure"
        , "Basel"
        , "Before"
        , "Belladonna"
        , "Bells"
        , "Berg"
        , "Blake"
        , "Blooming"
        , "Blue"
        , "Borges"
        , "Bravo"
        , "Breaks"
        , "Breeze"
        , "Keter"
        , "Phi"
        , "Bridge"
        , "Brink"
        , "Cadenza"
        , "Calaclysm"
        , "Calamity"
        , "Calibre"
        , "Callista"
        , "Careless"
        , "Carmichael"
        , "Carta"
        , "Celestial"
        , "Celia"
        , "Ceres"
        , "Chambers"
        , "Chanbara"
        , "Chandra"
        , "Char"
        , "Chasm"
        , "Cherise"
        , "Chrome"
        , "Claire"
        , "Clarity"
        , "Clearly"
        , "Cobra"
        , "Collapse"
        , "Congregation"
        , "Cordova"
        , "Corrected"
        , "Cosmos"
        , "Crimson"
        , "Croft"
        , "Crows"
        , "Cruz"
        , "Daisy"
        , "Daryl"
        , "Dawn"
        , "December"
        , "Dionysis"
        , "Diya"
        , "Dragoon"
        , "Drei"
        , "EX//HALE"
        , "Eager"
        , "Edelweiss"
        , "Eerie"
        , "Endings"
        , "Enoshima"
        , "Erin"
        , "Errant"
        , "Erykah"
        , "Eternity"
        , "Etienne"
        , "Evergreen"
        , "Exceed"
        , "Fading"
        , "Fall"
        , "Fantom"
        , "February"
        , "Fedor"
        , "Fee"
        , "Foon"
        , "Forever"
        , "Forgotten"
        , "Fortune"
        , "Four"
        , "Fulfilled"
        , "Galen"
        , "Gary"
        , "Gently"
        , "Ghostly"
        , "Gilbres"
        , "Giren"
        , "Glass"
        , "Gleaming"
        , "Glory"
        , "Glyph"
        , "Golden"
        , "Grace"
        , "Halls"
        , "Hasma"
        , "Heaven"
        , "Hectare"
        , "Helical"
        , "Henk"
        , "Hermitian"
        , "Heron"
        , "Hime"
        , "Honk"
        , "Hu"
        , "Hundred"
        , "Iapetus"
        , "Ismael"
        , "January"
        , "Jazz"
        , "Jennifer"
        , "Joon"
        , "Jorge"
        , "Journal"
        , "Joven"
        , "Joy"
        , "July"
        , "June"
        , "Juniper"
        , "Justice"
        , "Karen"
        , "Kaz"
        , "Kazmir"
        , "Keble"
        , "Keith"
        , "Kiba"
        , "Kilbride"
        , "Kim"
        , "Kingdom"
        , "Kitty"
        , "Kuro"
        , "Kwame"
        , "Lamar"
        , "Laplace"
        , "Lars"
        , "Last"
        , "Leaves"
        , "Links"
        , "Long"
        , "Luna"
        , "Mako"
        , "Makoto"
        , "Mal"
        , "March"
        , "Marida"
        , "Maryam"
        , "Matilde"
        , "May"
        , "Medea"
        , "Meisin"
        , "Meryuem"
        , "Metastasis"
        , "Millia"
        , "Mira"
        , "Mishka"
        , "Momo"
        , "Mu"
        , "Mugen"
        , "Mulder"
        , "Nadiya"
        , "Nakamura"
        , "Nana"
        , "Nat"
        , "Near"
        , "Never"
        , "Nine"
        , "November"
        , "Nu"
        , "Nyanna"
        , "Olive"
        , "Ong"
        , "Oratorio"
        , "Oskar"
        , "Palace"
        , "Park"
        , "Pathos"
        , "Pechen"
        , "Peel"
        , "Pelenor"
        , "Perennial"
        , "Perrin"
        , "Pewter"
        , "Phonon"
        , "Phylum"
        , "Pistil"
        , "Playa"
        , "Pliskin"
        , "Plue"
        , "Pool"
        , "Preet"
        , "Priest"
        , "Q"
        , "Quan"
        , "Rabbit"
        , "Rabia"
        , "Rachael"
        , "Rainy"
        , "Rashid"
        , "Reclaim"
        , "Redondo"
        , "Reed"
        , "Regrets"
        , "Remembrance"
        , "Remilia"
        , "Remy"
        , "Ren"
        , "Replica"
        , "Reppa"
        , "Restitution"
        , "Returner"
        , "Reuben"
        , "Reverie"
        , "Rose"
        , "Rossum"
        , "Ruby"
        , "Rye"
        , "Sachiko"
        , "Sacrament"
        , "Sacrosanct"
        , "Saif"
        , "San"
        , "Sanctity"
        , "Saturday"
        , "Scarlet"
        , "Schism"
        , "Schnee"
        , "Seconds"
        , "Seisma"
        , "Seitur"
        , "September"
        , "Seven"
        , "Shadows"
        , "Shanaya"
        , "Shin"
        , "Sigma"
        , "Siya"
        , "Sleipnir"
        , "Slide"
        , "Sofia"
        , "Softly"
        , "Solace"
        , "Solari"
        , "Song"
        , "Sonon"
        , "Sora"
        , "Sorrel"
        , "Spring"
        , "Stand"
        , "Summer"
        , "Sunny"
        , "Sword"
        , "Sylph"
        , "Søren"
        , "Tangent"
        , "Temerity"
        , "Tethys"
        , "Thalia"
        , "Theodor"
        , "Theta"
        , "Tiger"
        , "Today"
        , "Twain"
        , "Twelve"
        , "Two"
        , "Ultima"
        , "Unbridled"
        , "Underhand"
        , "Untimely"
        , "Uratora"
        , "Vidal"
        , "Villalobos"
        , "Vital"
        , "Void"
        , "Volver"
        , "Vrai"
        , "Walpurgis"
        , "Warren"
        , "Wasim"
        , "Watari"
        , "Weiss"
        , "Wing"
        , "Winter"
        , "Without"
        , "Woo"
        , "X"
        , "Xi"
        , "Xiao"
        , "Yang"
        , "Yubi"
        , "Zenith"
        , "Zhou"
        , "k"
        , "ϱ"
        ]


newConnector : Generator Connector
newConnector =
    Random.weighted
        ( 110, BreakingNonCombinator " " )
        [ ( 10, NonBreakingCombinator "-" )
        , ( 5, BreakingNonCombinator "-" )
        , ( 5, NonBreakingCombinator "'" )
        , ( 5, BreakingNonCombinator "'" )
        , ( 5, BreakingCombinator " d'" )
        , ( 5, BreakingCombinator " la " )
        , ( 5, NonBreakingCombinator " of " )
        , ( 5, BreakingCombinator " of " )
        , ( 5, BreakingNonCombinator " of " )
        , ( 5, BreakingCombinator " v'" )
        , ( 5, NonBreakingCombinator " v'" )
        , ( 5, BreakingCombinator " de " )
        , ( 2, BreakingCombinator " whist-" )
        , ( 2, BreakingCombinator " van " )
        , ( 2, BreakingCombinator " von " )
        , ( 1, NonBreakingCombinator " the " )
        , ( 1, BreakingNonCombinator ", " )
        , ( 1, BreakingNonCombinator " but " )
        , ( 1, BreakingNonCombinator " with " )
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
                                BreakingCombinator y ->
                                    case newNames of
                                        z :: zs ->
                                            case String.startsWith " " y of
                                                True ->
                                                    ( topName :: String.trimLeft (y ++ z) :: zs, BreakingNonCombinator " " :: newSeps )

                                                False ->
                                                    ( topName :: String.trimLeft (y ++ z) :: zs, newSeps )

                                        _ ->
                                            ( topName :: newNames, newSeps )

                                BreakingNonCombinator _ ->
                                    ( topName :: newNames, topSep :: newSeps )

                                NonBreakingCombinator y ->
                                    case newNames of
                                        z :: zs ->
                                            ( (topName ++ y ++ z) :: zs, newSeps )

                                        zs ->
                                            ( topName :: zs, newSeps )

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
                Just (BreakingCombinator x) ->
                    String.trim (x ++ baseName)

                Just (BreakingNonCombinator x) ->
                    String.trim (x ++ baseName)

                Just (NonBreakingCombinator x) ->
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


type Connector
    = BreakingCombinator String
    | BreakingNonCombinator String
    | NonBreakingCombinator String


getSepString con =
    case con of
        BreakingCombinator x ->
            x

        BreakingNonCombinator x ->
            x

        NonBreakingCombinator x ->
            x


newRelativeName : Name -> Generator Name
newRelativeName name =
    case name of
        Name { names, separators, family, given } ->
            case List.length names of
                1 ->
                    newName

                x ->
                    Random.map
                        (\newNames ->
                            Name
                                { names =
                                    List.concat
                                        [ List.take family newNames
                                        , List.take 1 (List.drop family names)
                                        , List.drop family newNames
                                        ]
                                , separators = separators
                                , family = family
                                , given = given
                                }
                        )
                        (Random.list (x - 1) getNameFromList)


newName : Generator Name
newName =
    Random.map
        (\n ->
            if n < 1 then
                1

            else
                round n
        )
        (Random.Float.normal 2.4 0.6)
        |> Random.andThen (\len -> Random.list len getNameFromList)
        |> Random.andThen
            (\xs ->
                Random.map
                    (\cons -> coalesce xs cons)
                    (Random.list (List.length xs - 1) newConnector)
            )
        |> Random.andThen
            (\( xs, cons ) ->
                Random.map
                    (\fam -> ( xs, cons, fam ))
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
