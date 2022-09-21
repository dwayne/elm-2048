module App.Data.Tally exposing
  ( Tally, zero
  , getCurrent, getBest
  , resetCurrent, addPoints
  )


import App.Data.Points as Points exposing (Points)


type Tally
  = Tally Data


type alias Data =
  { current : Points
  , best : Points
  }


zero : Tally
zero =
  Tally
    { current = Points.zero
    , best = Points.zero
    }


getCurrent : Tally -> Points
getCurrent (Tally { current }) =
  current


getBest : Tally -> Points
getBest (Tally { best }) =
  best


resetCurrent : Tally -> Tally
resetCurrent (Tally tally) =
  Tally { tally | current = Points.zero }


addPoints : Points -> Tally -> Tally
addPoints points (Tally { current, best }) =
  let
    newCurrent =
      Points.add points current
  in
  Tally
    { current = newCurrent
    , best = Points.max newCurrent best
    }
