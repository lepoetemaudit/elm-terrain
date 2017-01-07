module Utils exposing (..)

import String exposing (fromChar)

formatFloat : String -> String
formatFloat val =
  String.foldl (\el (str, dist) ->
    if dist == -1 then
      case el of
        '.' -> ((str ++ "."), 0)
        _ -> ((str ++ fromChar el), -1)
    else if dist < 2 then
      ((str ++ String.fromChar el), (dist+1))
    else (str, dist)
    ) ("", -1) val
    |> Tuple.first