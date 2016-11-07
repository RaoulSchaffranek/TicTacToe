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
          [viewField (0,0) "top left" history
          ,viewField (1,0) "top center" history
          ,viewField (2,0) "top right" history]
        ,tr []
          [viewField (0,1) "middle left" history
          ,viewField (1,1) "middle center" history
          ,viewField (2,1) "middle right" history]
        ,tr []
          [viewField (0,2) "bottom left" history
          ,viewField (1,2) "bottom center" history
          ,viewField (2,2) "bottom right" history]]]]


viewField : Point -> String -> History -> Html Msg
viewField p title' history =
  let
    player =
      List.head (List.filter (\(player, p') -> p' == p) (who history))
  in
    case player of
      Nothing     ->
        td [title (title' ++ ": free")]
          [button [onClick (Pick p), title title', class "ttt-field-button"]
            [text ("Pick " ++ title')]]
      Just (X, _) ->
        td [title (title' ++ ": X")] [text "✕"]
      Just (O, _) ->
        td [title (title' ++ ": O")] [text "⭘"]

viewWinner : History -> Html Msg
viewWinner history =
  case winner history of
    Nothing -> if List.length history == 9
      then button
        [onClick Reset, class "ttt-status ttt-draw", title "start new game", autofocus True]
        [h1 [] [text "⚔ draw ⚔"]]
      else div [class "ttt-status ttt-ongoing"] []
    Just player -> case player of
      X -> button
        [onClick Reset, class "ttt-status ttt-win-x", title "start new game", autofocus True]
        [h1 [] [text "👑 ✕ wins 👑"]]
      O -> button
        [onClick Reset, class "ttt-status ttt-win-o", title "start new game", autofocus True]
        [h1 [] [text "👑 ⭘ wins 👑"]]