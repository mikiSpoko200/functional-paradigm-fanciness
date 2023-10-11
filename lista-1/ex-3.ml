let hd stream = stream 0
let tl stream index = stream (index + 1)
let add stream const index = stream index + const
let map stream f index = f (stream index)

(* map może zbudować add *)
let map_add stream const = map stream (fun x -> x + const)
let map2 f s1 s2 index = f (s1 index) (s2 index)
let replace n value stream index = if index = n then value else stream index
let take_every n s index = s (n * index)
