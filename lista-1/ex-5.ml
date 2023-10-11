let tabulate stream ?(start_index=0) end_index =
  let rec inner acc index =
      if start_index > index then
          acc
      else
          inner (stream index :: acc) (index - 1)
  in inner [] end_index
;;
