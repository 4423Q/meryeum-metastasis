module Stats exposing
    ( InvisibleStats
    , Stat
    , VisibleStats
    , newInvisibleStats
    , newStat
    , newVisibleStats
    , relativeInvisStatsUpdate
    , relativeStatsUpdate
    )

import Random exposing (Generator)
import Random.Float


type alias Stat =
    Int


newStat : Generator Stat
newStat =
    Random.map
        (\n ->
            if n > 10 then
                10

            else if n < 0 then
                0

            else
                round n
        )
        (Random.Float.normal 5 2)


type alias VisibleStats =
    { sharp : Stat
    , danger : Stat
    , hot : Stat
    , extra : Stat
    }


type alias InvisibleStats =
    { horny : Stat
    , angry : Stat
    , ambition : Stat
    , diversion : Stat
    }


relativeStatsUpdate : VisibleStats -> VisibleStats -> VisibleStats
relativeStatsUpdate orig delta =
    { orig
        | sharp = clamp 0 10 (orig.sharp + delta.sharp)
        , danger = clamp 0 10 (orig.danger + delta.danger)
        , hot = clamp 0 10 (orig.hot + delta.hot)
        , extra = clamp 0 10 (orig.extra + delta.extra)
    }


relativeInvisStatsUpdate : InvisibleStats -> InvisibleStats -> InvisibleStats
relativeInvisStatsUpdate orig delta =
    { orig
        | horny = clamp 0 10 (orig.horny + delta.horny)
        , angry = clamp 0 10 (orig.angry + delta.angry)
        , ambition = clamp 0 10 (orig.ambition + delta.ambition)
        , diversion = clamp 0 10 (orig.diversion + delta.diversion)
    }


newVisibleStats =
    Random.map4
        (\a b c d -> { sharp = a, danger = b, hot = c, extra = d })
        newStat
        newStat
        newStat
        newStat


newInvisibleStats =
    Random.map4
        (\a b c d -> { horny = a, angry = b, ambition = c, diversion = d })
        newStat
        newStat
        newStat
        newStat
