let mysterious =
  let rec inner x = match x with [] -> inner [] | a :: t -> a in
  inner []

let even_more_mysterious x = mysterious
