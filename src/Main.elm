module Main exposing (Model, main)

import Bonds
import Browser
import Classes
import Debug
import Dict exposing (Dict)
import Html exposing (Html, a, button, div, form, input, text)
import Html.Attributes exposing (href, style, value)
import Html.Events exposing (onClick, onInput, onSubmit)
import Interactions
import List.Extra
import Pronouns
import Quirks
import Random
import String.Extra
import Student
import StudentBody
import Util
import Weapon


main =
    Browser.element { init = init, update = update, subscriptions = subscriptions, view = view }


type alias Model =
    { body : StudentBody.StudentBody
    , events : List Interactions.Event
    , editingNicks : Dict Int String
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { body = StudentBody.empty
      , events = []
      , editingNicks = Dict.empty
      }
    , Random.generate
        NewStudents
        (StudentBody.addStudents 3 StudentBody.empty)
    )



-- MSG


type NicknameMsg
    = Start
    | Stop
    | Update String
    | Add String
    | Remove


type Msg
    = NoOp
    | GetInteractions StudentBody.StudentBody Student.Student
    | GetAllInteractions StudentBody.StudentBody
    | GenInteractions StudentBody.StudentBody
    | GenEvents StudentBody.StudentBody
    | MaybeNewStudents (Maybe StudentBody.StudentBody)
    | NewStudents StudentBody.StudentBody
    | NewInteractions (List Interactions.Interaction)
    | NewEvents ( List Interactions.Event, StudentBody.StudentBody )
    | GetNewStudents Int
    | GetNewRelative Int
    | Nickname Int NicknameMsg


getNewRelative : StudentBody.StudentBody -> Int -> Cmd Msg
getNewRelative body id =
    let
        newStudentBody =
            StudentBody.addRelative id body
    in
    case newStudentBody of
        Nothing ->
            Cmd.none

        Just x ->
            Random.generate MaybeNewStudents x



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ events, body, editingNicks } as model) =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        Nickname i x ->
            case x of
                Start ->
                    ( { model
                        | editingNicks = Dict.insert i "" editingNicks
                      }
                    , Cmd.none
                    )

                Update s ->
                    ( { model
                        | editingNicks =
                            Dict.update i (\_ -> Just s) editingNicks
                      }
                    , Cmd.none
                    )

                Remove ->
                    ( StudentBody.updateStudentById
                        (\std -> Student.setNickname std Nothing)
                        body
                        i
                        |> Maybe.map
                            (\newBody ->
                                { model
                                    | body = newBody
                                }
                            )
                        |> Maybe.withDefault model
                    , Cmd.none
                    )

                Add s ->
                    ( StudentBody.updateStudentById
                        (\std -> Student.setNickname std (Just s))
                        body
                        i
                        |> Maybe.map
                            (\newBody ->
                                { model
                                    | body = newBody
                                    , editingNicks = Dict.remove i editingNicks
                                }
                            )
                        |> Maybe.withDefault model
                    , Cmd.none
                    )

                Stop ->
                    ( { model
                        | editingNicks = Dict.remove i editingNicks
                      }
                    , Cmd.none
                    )

        MaybeNewStudents (Just x) ->
            ( { model | body = x }, Cmd.none )

        MaybeNewStudents Nothing ->
            ( model, Cmd.none )

        NewStudents x ->
            ( { model | body = x }, Cmd.none )

        NewEvents ( x, y ) ->
            case ( x, y ) of
                _ ->
                    ( { model
                        | events = List.append x events
                        , body = y
                      }
                    , Cmd.none
                    )

        NewInteractions x ->
            case x of
                _ ->
                    ( model, Cmd.none )

        GetInteractions b student ->
            case StudentBody.findInteractions student b of
                _ ->
                    ( model, Cmd.none )

        GetAllInteractions b ->
            case StudentBody.findAllInteractions b of
                _ ->
                    ( model, Cmd.none )

        GenInteractions _ ->
            ( model, Random.generate NewInteractions (StudentBody.genWeeksInteractions body) )

        GenEvents _ ->
            ( model
            , Random.generate NewEvents
                (StudentBody.genWeeksInteractions body
                    |> Random.andThen (StudentBody.processWeeksInteractions body)
                )
            )

        GetNewStudents n ->
            ( model, Random.generate NewStudents (StudentBody.addStudents n body) )

        GetNewRelative id ->
            ( model, getNewRelative body id )


viewStats { danger, hot, sharp, extra } =
    let
        mapper =
            \( x, y ) -> div [] [ text (x ++ String.fromInt y) ]
    in
    div []
        (List.map
            mapper
            [ ( "dangerous - ", danger )
            , ( "hot - ", hot )
            , ( "sharp - ", sharp )
            , ( "extra - ", extra )
            ]
        )


quirkToString : Student.Student -> Quirks.Quirk -> String
quirkToString student name =
    let
        sProns =
            Student.getPronouns student

        friendly =
            Student.getFriendlyName student
    in
    case name of
        Quirks.HasADog ->
            String.Extra.toSentenceCase sProns.pos ++ " pet dog stays back home during term time."

        Quirks.LovesHotDogs ->
            "Really loves hot dogs. Way more than you'd expect."

        Quirks.KeepsABulletJournal ->
            "Keeps a bullet journal, neat!"

        Quirks.FightsBlindfolded ->
            "Fights blindfolded."

        Quirks.LovesToFight ->
            String.join " "
                [ String.Extra.toSentenceCase sProns.subj
                , Util.isare sProns.subj
                , "a known brawler."
                ]

        Quirks.StoleAMecha ->
            String.join " "
                [ friendly
                , "stole a classified war machine from a government facility."
                ]

        Quirks.DestinedForGreatness ->
            String.join " "
                [ String.Extra.toSentenceCase sProns.subj
                , "will one day do someting incredible... but not yet."
                ]

        Quirks.ExtraHot ->
            String.join " "
                [ "There's really no overstating how absurdly hot"
                , String.toLower sProns.subj
                , Util.isare sProns.subj ++ "..."
                ]

        Quirks.RulesNerd ->
            String.join " "
                [ String.Extra.toSentenceCase sProns.subj
                , Util.hasHave sProns.subj
                , "memorised the \"Battlefields & Ballistas\" ruleset, including the extended grappling supplement."
                ]

        Quirks.Homesick ->
            String.join " "
                [ String.Extra.toSentenceCase sProns.subj
                , Util.isare sProns.subj
                , "a little bit homesick :("
                ]

        Quirks.LiveStreamsTraining ->
            "Livestreams " ++ sProns.pos ++ " training sessions."

        Quirks.TeamCaptain ->
            "Captain of the school Vethball team."

        Quirks.Fanfic ->
            "Working on some new fanfic, some real premium shit..."

        Quirks.Illuminati ->
            "Inducted into the secret society that runs everything."

        Quirks.CatEars ->
            String.join " "
                [ String.Extra.toSentenceCase (Util.combHasHave sProns.subj)
                , "got kitty ears."
                ]

        Quirks.WolfEars ->
            String.join " "
                [ String.Extra.toSentenceCase (Util.combHasHave sProns.subj)
                , "got wolf ears. Awoo~~~"
                ]


bondToString : Student.Student -> Student.Student -> Bonds.Bond -> String
bondToString source target bond =
    let
        sProns =
            Student.getPronouns source

        tName =
            Student.getName target

        sFriendly =
            Student.getFriendlyName source
    in
    case bond of
        Bonds.Relative Bonds.Sibling _ ->
            String.Extra.toSentenceCase sProns.pos ++ " sibling is " ++ tName ++ "."

        Bonds.Relative Bonds.Cousin _ ->
            String.Extra.toSentenceCase sProns.subj ++ " " ++ Util.isare sProns.subj ++ " " ++ tName ++ "'s cousin."

        Bonds.InLove _ ->
            String.Extra.toSentenceCase sProns.subj ++ " " ++ Util.isare sProns.subj ++ " in love with " ++ tName ++ "!"

        Bonds.Friend _ ->
            String.Extra.toSentenceCase sProns.subj ++ " " ++ Util.isare sProns.subj ++ " friends with " ++ tName ++ "."

        Bonds.Enemy _ ->
            String.Extra.toSentenceCase sProns.subj ++ " " ++ Util.deplural sProns.subj "despises" ++ " " ++ tName ++ "."

        Bonds.Rival _ ->
            tName ++ " has a rivalry with " ++ sFriendly ++ "."

        Bonds.Lustful _ ->
            sFriendly ++ " gets flustered by " ++ tName ++ "."

        Bonds.Admires _ ->
            String.Extra.toSentenceCase sProns.subj ++ " " ++ Util.deplural sProns.subj "thinks" ++ " " ++ tName ++ " is like... really great."



-- TODO Switch to rendering by bond to rendering by targeted students


renderBond : Student.Student -> ( Bonds.Bond, Student.Student ) -> Html Msg
renderBond studentSource ( bond, studentTarget ) =
    div []
        [ text <| bondToString studentSource studentTarget bond ]


renderQuirk : Student.Student -> Quirks.Quirk -> Html Msg
renderQuirk student quirk =
    div []
        [ text <| quirkToString student quirk ]


renderStudent : StudentBody.StudentBody -> Dict Int String -> Student.Student -> Html Msg
renderStudent body editingNicks x =
    let
        quirks =
            Student.getQuirks x

        bonds =
            Student.getBonds x
                |> List.foldr
                    (\val acc ->
                        case StudentBody.getBondTarget val body of
                            Just y ->
                                ( val, y ) :: acc

                            Nothing ->
                                acc
                    )
                    []

        stats =
            Student.getStats x

        prons =
            Student.getPronouns x

        weptext =
            case Student.getWeapon x of
                Weapon.Pilot machine ->
                    Util.ana machine ++ " " ++ machine ++ " pilot"

                Weapon.Soldier ->
                    "some kinda soldier"

                Weapon.Commander ->
                    "a Future Commander"

                Weapon.Tactician ->
                    "a Tactician in Training"

                Weapon.Special ->
                    "something special !!!"

                Weapon.Weapon wep ->
                    "a person who is also " ++ Util.ana wep ++ " " ++ wep
    in
    div []
        [ div [] [ text ("Name: " ++ Student.getName x ++ " [#" ++ String.fromInt (Student.getNumber x) ++ "]" ++ " (" ++ Util.slashmode prons ++ ")") ]
        , div []
            (case
                Dict.get (Student.getNumber x) editingNicks
             of
                Just result ->
                    [ text "Nickname:"
                    , form
                        [ onSubmit (Nickname (Student.getNumber x) (Add result)) ]
                        [ input
                            [ value result
                            , onInput (\nick -> Nickname (Student.getNumber x) (Update nick))
                            ]
                            []
                        ]
                    ]

                Nothing ->
                    case Student.getNickname x of
                        Just n ->
                            [ text ("Nickname: " ++ n), button [ onClick (Nickname (Student.getNumber x) Remove) ] [ text "Remove??" ] ]

                        Nothing ->
                            [ text "No nickname", button [ onClick (Nickname (Student.getNumber x) Start) ] [ text "Add?" ] ]
            )
        , viewStats stats
        , div [] [ text ("Classes: " ++ (Student.getClasses x |> List.map Classes.toString |> String.join ", ")) ]
        , div []
            [ text (Student.getFriendlyName x ++ " is " ++ weptext)
            ]
        , div [] (List.map (renderBond x) bonds)
        , div [] (List.map (renderQuirk x) quirks)
        , div []
            [ button
                [ onClick (GetInteractions body x) ]
                [ text "DEBUG INTERACTIONS" ]
            ]
        ]


resultsToString : (Int -> Maybe Student.Student) -> List Interactions.Result -> String
resultsToString stdGetter results =
    let
        sm id func =
            stdGetter id |> Maybe.map func

        gn =
            Student.getName
    in
    results
        |> List.Extra.gatherEqualsBy Interactions.getResultTarget
        |> List.map (\( x, y ) -> ( Interactions.getResultTarget x, x :: y ))
        |> List.map
            (\( id, results2 ) ->
                stdGetter id
                    |> Maybe.map
                        (\x ->
                            Student.getName x
                                ++ " "
                                ++ (results2
                                        |> List.map
                                            (\y ->
                                                case y of
                                                    Interactions.BondsFormed _ xs ->
                                                        xs
                                                            |> List.map
                                                                (\bond ->
                                                                    case bond of
                                                                        Bonds.Friend i ->
                                                                            sm i (\t -> "made friends with " ++ gn t)

                                                                        Bonds.InLove i ->
                                                                            sm i (\t -> "fell in love with " ++ gn t)

                                                                        Bonds.Lustful i ->
                                                                            sm i (\t -> "developed a crush on " ++ gn t)

                                                                        Bonds.Enemy i ->
                                                                            sm i (\t -> "got pissed off at " ++ gn t)

                                                                        Bonds.Rival i ->
                                                                            sm i (\t -> "started a rivalry with " ++ gn t)

                                                                        Bonds.Admires i ->
                                                                            sm i (\t -> "was impressed by " ++ gn t)

                                                                        _ ->
                                                                            Nothing
                                                                )

                                                    Interactions.BondsBroken _ xs ->
                                                        xs
                                                            |> List.map
                                                                (\bond ->
                                                                    case bond of
                                                                        Bonds.Friend i ->
                                                                            stdGetter i
                                                                                |> Maybe.map
                                                                                    (\target ->
                                                                                        "is no longer friends with " ++ Student.getName target
                                                                                    )

                                                                        Bonds.InLove i ->
                                                                            stdGetter i
                                                                                |> Maybe.map
                                                                                    (\target ->
                                                                                        "is no longer in love with " ++ Student.getName target
                                                                                    )

                                                                        Bonds.Lustful i ->
                                                                            stdGetter i
                                                                                |> Maybe.map
                                                                                    (\target ->
                                                                                        "is no longer horny for " ++ Student.getName target
                                                                                    )

                                                                        Bonds.Admires i ->
                                                                            stdGetter i
                                                                                |> Maybe.map
                                                                                    (\target ->
                                                                                        "doesn't think so much of " ++ Student.getName target ++ " any more"
                                                                                    )

                                                                        Bonds.Enemy i ->
                                                                            stdGetter i
                                                                                |> Maybe.map
                                                                                    (\target ->
                                                                                        "made up with " ++ Student.getName target
                                                                                    )

                                                                        Bonds.Rival i ->
                                                                            stdGetter i
                                                                                |> Maybe.map
                                                                                    (\target ->
                                                                                        "ended a rivalry with " ++ Student.getName target
                                                                                    )

                                                                        _ ->
                                                                            Nothing
                                                                )

                                                    _ ->
                                                        []
                                            )
                                        |> List.map Util.removeNothings
                                        |> List.concat
                                        |> Util.commasAnd
                                   )
                        )
            )
        |> Util.removeNothings
        |> String.join ". "


renderEvent : StudentBody.StudentBody -> Interactions.Event -> Html Msg
renderEvent body event =
    let
        qualitify =
            List.Extra.gatherEqualsBy Tuple.second
                >> List.map
                    (\( ( v, qual ) as a, vs ) ->
                        (a :: vs)
                            |> List.map (Tuple.first >> Student.getFriendlyName)
                            |> Util.commasAnd
                            |> (\x ->
                                    String.join " "
                                        [ x
                                        , "had"
                                        , Util.ana (Interactions.qualityToString qual)
                                        , Interactions.qualityToString qual
                                        , "time"
                                        ]
                               )
                    )

        getStudent =
            StudentBody.getStudentById body

        getParticipants =
            List.map (Tuple.mapFirst (StudentBody.getStudentById body))
                >> List.map
                    (\( x, y ) ->
                        case x of
                            Just z ->
                                Just ( z, y )

                            Nothing ->
                                Nothing
                    )
                >> List.foldr
                    (\val acc ->
                        val
                            |> Maybe.map (\v2 -> v2 :: acc)
                            |> Maybe.withDefault acc
                    )
                    []
    in
    div []
        [ case event of
            Interactions.ClassEvent class results ->
                Classes.toString class
                    ++ " happened."
                    |> (\x -> x ++ " " ++ resultsToString getStudent results)
                    |> text

            Interactions.HangoutEvent xs rs ->
                let
                    participants =
                        getParticipants xs
                in
                participants
                    |> List.map (\x -> x |> Tuple.first |> Student.getName)
                    |> Util.commasAnd
                    |> (\x -> x ++ " hung out together")
                    |> (\starter ->
                            participants
                                |> qualitify
                                |> (\x -> String.join ", " (starter :: x))
                       )
                    |> (\x -> x ++ ".")
                    |> (\x -> x ++ " " ++ resultsToString getStudent rs)
                    |> text

            Interactions.DateEvent xs rs ->
                let
                    participants =
                        getParticipants xs
                in
                participants
                    |> List.map (\x -> x |> Tuple.first |> Student.getName)
                    |> Util.commasAnd
                    |> (\x -> x ++ " went on a date")
                    |> (\starter ->
                            participants
                                |> qualitify
                                |> (\x -> starter ++ ": " ++ Util.commasAnd x)
                       )
                    |> (\x -> x ++ ".")
                    |> (\x -> x ++ " " ++ resultsToString getStudent rs)
                    |> text

            Interactions.TransitionEvent s t _ ->
                s
                    |> getStudent
                    |> Maybe.map
                        (\x ->
                            let
                                prons =
                                    Student.getPronouns x
                            in
                            Student.getName x
                                ++ (case t of
                                        Interactions.NameT y ->
                                            " changed " ++ prons.pos ++ " name."

                                        Interactions.PronT y ->
                                            " started using " ++ Pronouns.slashmode y ++ " pronouns."

                                        Interactions.BothT y z ->
                                            " changed " ++ prons.pos ++ " name and started using " ++ Pronouns.slashmode z ++ " pronouns."
                                   )
                        )
                    |> Maybe.withDefault
                        ("Something went wrong attempting to get student: "
                            ++ String.fromInt s
                        )
                    |> text
        ]


view : Model -> Html Msg
view { events, body, editingNicks } =
    let
        y =
            body
    in
    div []
        [ a [ href "https://twitter.com/4423QQ" ] [ text "by edelweiss (4423)" ]
        , div []
            [ button [ onClick (GetNewStudents 1) ] [ text "NEW PERSON !!!!" ]
            , button [ onClick (GenInteractions y) ] [ text "DEBUG ALL INTERACTIONS" ]
            , button [ onClick (GenEvents y) ] [ text "DEBUG MOVE TO FUTURE" ]
            ]
        , div [ style "display" "flex" ]
            [ div [ style "flex" "1 1 0" ]
                (StudentBody.asList y
                    |> List.sortBy (\x -> 1 - Student.getNumber x)
                    |> List.map (renderStudent y editingNicks)
                    |> List.intersperse (div [] [ text "------" ])
                )
            , div [ style "flex" "1 1 0" ] (List.map (renderEvent body) (List.take 20 events))
            ]
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
