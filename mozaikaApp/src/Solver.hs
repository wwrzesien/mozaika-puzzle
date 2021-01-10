module Solver where

-- replace elem in nth position in row
replace::Int->a->[a]->[a]
replace _ _ [] = []
replace 0 a (_:xs) = a:xs
replace n a (x:xs) =
  if n < 0
    then (x:xs)
    else x: replace (n-1) a xs

-- replace elem in spec position in 2d array
modify_elem::Int->Int->Char->[String]->[String]
modify_elem row col a xs =
    let row_to_mod = xs !! row
        modified_row = replace col a row_to_mod
    in replace row modified_row xs