module Game.List exposing (cartesianMap)


-- Given n >= 0, m >= 0 and a function, f, it returns a list of the form
-- [ f 0 0, f 0 1, ..., f i j, ..., f (n - 1) (m - 1) ] such that 0 <= i < n
-- and 0 <= j < m.
--
-- For e.g.
--
-- cartesianMap 2 3 (,)
-- => [ (0, 0), (0, 1), (0, 2), (1, 0), (1, 1), (1, 2) ]
--
-- cartesianMap 2 3 (+)
-- => [ 0, 1, 2, 1, 2, 3 ]
cartesianMap : Int -> Int -> (Int -> Int -> a) -> List a
cartesianMap n m f =
  let
    iter i j =
      if i == n then
        []
      else if j == m then
        iter (i+1) 0
      else
        f i j :: iter i (j+1)
  in
    iter 0 0
