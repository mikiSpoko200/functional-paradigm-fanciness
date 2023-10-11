let f a s = let rec inner index = if index > 0 then f (inner index - 1) s index else f a (s 0)
in fun index -> inner index
