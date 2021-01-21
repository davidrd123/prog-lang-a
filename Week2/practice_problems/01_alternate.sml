fun alternate (xs : int list) =
    if null xs
    then 0
    else hd(xs) - alternate(tl xs)
