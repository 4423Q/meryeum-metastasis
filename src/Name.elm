module Name exposing
    ( Name
    , newName
    , newRelativeName
    , testDefault
    , toFamilyString
    , toGivenString
    , toString
    )

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
        , "Acrid"
        , "Addax"
        , "Aegis"
        , "Aether"
        , "Ai"
        , "Algernon"
        , "Allie"
        , "Alpha"
        , "Altar"
        , "Alvarez"
        , "Amar"
        , "Amuro"
        , "Apex"
        , "Aphid"
        , "Aphor"
        , "April"
        , "Arif"
        , "Art"
        , "Arturia"
        , "Asano"
        , "Atrophy"
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
        , "Bridge"
        , "Brink"
        , "Cadaver"
        , "Cadenza"
        , "Calaclysm"
        , "Calamity"
        , "Calibre"
        , "Callista"
        , "Careless"
        , "Carmichael"
        , "Carta"
        , "Cecil"
        , "Celestial"
        , "Celia"
        , "Celica"
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
        , "Corin"
        , "Corrected"
        , "Cosmos"
        , "Creed"
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
        , "Dost"
        , "Dragoon"
        , "Drei"
        , "Dusk"
        , "EX//HALE"
        , "Eager"
        , "Edelweiss"
        , "Eerie"
        , "Ember"
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
        , "Imran"
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
        , "Keter"
        , "Kiba"
        , "Kilbride"
        , "Killian"
        , "Kim"
        , "Kingdom"
        , "Kitty"
        , "Kubra"
        , "Kuro"
        , "Kwame"
        , "Lamar"
        , "Laplace"
        , "Lars"
        , "Last"
        , "Leaves"
        , "Levar"
        , "Links"
        , "Long"
        , "Luna"
        , "Mako"
        , "Makoto"
        , "Mal"
        , "Mao"
        , "March"
        , "Marida"
        , "Maryam"
        , "Matilde"
        , "May"
        , "Medea"
        , "Meisin"
        , "Meryuem"
        , "Metastasis"
        , "Miles"
        , "Milieu"
        , "Millia"
        , "Mir"
        , "Mira"
        , "Miralia"
        , "Mishka"
        , "Momo"
        , "Mu"
        , "Mugen"
        , "Mulder"
        , "Nadiya"
        , "Nakamura"
        , "Nana"
        , "Narrative"
        , "Nat"
        , "Near"
        , "Neta"
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
        , "Phi"
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
        , "Qeon"
        , "Quan"
        , "Rabbit"
        , "Rabia"
        , "Rachael"
        , "Rainy"
        , "Ran"
        , "Rashid"
        , "Reclaim"
        , "Redondo"
        , "Reed"
        , "Refrain"
        , "Regrets"
        , "Remembrance"
        , "Remilia"
        , "Reminiscence"
        , "Remy"
        , "Ren"
        , "Replica"
        , "Reppa"
        , "Respite"
        , "Restitution"
        , "Retort"
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
        , "Sadiq"
        , "Saif"
        , "Salim"
        , "San"
        , "Sanctity"
        , "Sappho"
        , "Saturday"
        , "Scarlet"
        , "Schism"
        , "Schnee"
        , "Seaborg"
        , "Seconds"
        , "Seisma"
        , "Seitur"
        , "Selene"
        , "September"
        , "Setsuna"
        , "Seven"
        , "Shadows"
        , "Shalifa"
        , "Shanaya"
        , "Shin"
        , "Sigma"
        , "Silver"
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
        , "Stronghold"
        , "Summer"
        , "Sunny"
        , "Sunset"
        , "Sword"
        , "Sylph"
        , "Søren"
        , "Tangent"
        , "Temerity"
        , "Terra"
        , "Tethys"
        , "Thalia"
        , "Tharsis"
        , "Theodor"
        , "Theta"
        , "Thornton"
        , "Tiger"
        , "Today"
        , "Tower"
        , "Twain"
        , "Twelve"
        , "Two"
        , "Ultima"
        , "Unbridled"
        , "Underhand"
        , "Untimely"
        , "Uratora"
        , "Valdez"
        , "Vesper"
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
        , "Wong"
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


testDefault =
    Name
        { names = [ "a", "b" ]
        , separators = []
        , family = 1
        , given = 0
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
