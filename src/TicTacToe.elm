import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Html.App as App


main : Program Never
main =
  App.beginnerProgram { model = [], view = view, update = update }

-- Types


type Player
  = X
  | O


type Msg
  = Reset
  | Pick Point


type alias Point =
  (Int, Int)


type alias History =
  List Point


-- MODEL

who : History -> List (Player, Point)
who history =
  List.indexedMap 
    (\i point ->
      if i % 2 == 0
        then (X, point)
        else (O, point)) history

winner : History -> Maybe Player
winner history =
  let
    winSituations : List (Maybe Player)
    winSituations =
      [ whoWins (\(_, (_, y)) -> y == 0)    -- row 0
      , whoWins (\(_, (_, y)) -> y == 1)    -- row 1
      , whoWins (\(_, (_, y)) -> y == 2)    -- row 2
      , whoWins (\(_, (x, _)) -> x == 0)    -- column 1
      , whoWins (\(_, (x, _)) -> x == 1)    -- column 2
      , whoWins (\(_, (x, _)) -> x == 2)    -- column 3
      , whoWins (\(_, (x, y)) -> x == y)    -- diagonal 1
      , whoWins (\(_, (x, y)) -> x == 2-y)] -- diagonal 2

    whoWins : ((Player, Point) -> Bool) -> Maybe Player
    whoWins predicate =
      whoWins'
        (List.map (\(player,_) -> player)
        (List.filter predicate (who history)))

    whoWins' : List Player -> Maybe Player
    whoWins' situation =
      case situation of
        [a, b, c] -> if (a == b && b == c)
          then Just a
          else Nothing
        _ -> Nothing

    in
      Maybe.oneOf winSituations


-- UPDATE


update : Msg -> History -> History
update msg history =
  case msg of
    Reset -> []

    Pick point -> if List.member point history
      then history
      else history ++ [point]


-- VIEW


view : History -> Html Msg
view history =
   div []
    [ viewWinner history
    , table []
      [ tbody []
        [tr []
          [viewField (0,0) history
          ,viewField (1,0) history
          ,viewField (2,0) history]
        ,tr []
          [viewField (0,1) history
          ,viewField (1,1) history
          ,viewField (2,1) history]
        ,tr []
          [viewField (0,2) history
          ,viewField (1,2) history
          ,viewField (2,2) history]]]]


viewField : Point -> History -> Html Msg
viewField p history =
  let
    player =
      List.head (List.filter (\(player, p') -> p' == p) (who history))
    symbol = case player of
      Nothing    -> "\xFEFF"
      Just (X,_) -> "✕"
      Just (O,_) -> "⭘"
  in
    td [onClick (Pick p)] [text symbol]


viewWinner : History -> Html Msg
viewWinner history =
  case winner history of
    Nothing -> if List.length history == 9
      then div
        [onClick Reset, class "ttt-status ttt-draw"]
        [h1 [] [text "⚔ draw ⚔"]]
      else div [class "ttt-status ttt-ongoing"] []
    Just player -> case player of
      X -> div
        [onClick Reset, class "ttt-status ttt-win-x"]
        [h1 [] [text "👑 ✕ wins 👑"]]
      O -> div
        [onClick Reset, class "ttt-status ttt-win-o"]
        [h1 [] [text "👑 ⭘ wins 👑"]]