module Stats exposing (InvisibleStats, Stat, VisibleStats, newInvisibleStats, newStat, newVisibleStats)

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


newVisibleStats =
    Random.map4
        (\a b c d -> { sharp = a, danger = b, hot = c, extra = d })
        newStat
        newStat
        newStat
        newStat


type alias InvisibleStats =
    { horny : Stat
    , angry : Stat
    , ambition : Stat
    , diversion : Stat
    }


newInvisibleStats =
    Random.map4
        (\a b c d -> { horny = a, angry = b, ambition = c, diversion = d })
        newStat
        newStat
        newStat
        newStat
