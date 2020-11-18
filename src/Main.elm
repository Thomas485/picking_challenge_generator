module Main exposing (..)

import Browser
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Dropdown
import Dict exposing (Dict,empty)
import Tuple exposing (first,second)
import Base64 exposing (encode)
import Array exposing (Array)
import List.Extra exposing (takeWhile,dropWhile)
import Maybe exposing (andThen,withDefault)

type alias Model = {
    pinning: Array (String,String,String)
    ,dp: Dict String (Int,Int) -- (current,max)
    ,kp: Dict String (Int,Int) -- (current,max)
    -- springs not needed (enough)
    , encoded: Maybe String
    }

new_model = 
    { pinning= Array.fromList
        [ ("","","")
        , ("","","")
        , ("","","")
        , ("","","")
        , ("","","")
        ]
    , dp= Dict.fromList 
        [ ("Normal1",(0,5))
        , ("Normal2",(0,5))
        , ("Normal3",(0,5))
        , ("Serrated1",(0,7))
        , ("Serrated2",(0,7))
        , ("Spool",(0,4))
        , ("HalfSpool1",(0,5))
        , ("HalfSpool2",(0,5))
        , ("TPin",(0,7))
        , ("TwoPart",(0,7))
        ]
    , kp= Dict.fromList 
        [ ("P1",(0,2))
        , ("P2",(0,2))
        , ("P3",(0,2))
        , ("P4",(0,3))
        , ("P5",(0,4))
        , ("P6",(0,2))
        , ("P7",(0,4))
        , ("P8",(0,6))
        , ("P9",(0,2))
        , ("P10",(0,10))
        , ("P11",(0,7))
        ]
    , encoded = Nothing
    }

main =
  Browser.sandbox { init = new_model, update = update, view = view }

type Msg = Generate
    | SetKP Int String
    | SetDP Int String
    | SetSp Int String

tuple_empty (a,b,c) = String.isEmpty a && String.isEmpty b && String.isEmpty c

enc_cylinder pins = if tuple_empty pins then
    "null"
    else let (s,d,k) = pins in
        "[\""++s++"\",\""++d++"\",\""++k++"\"]"

enc_cylinders arr = String.join ","
    <| List.map enc_cylinder
    <| Array.toList arr
  
enc: Array (String,String,String) -> Maybe String
enc pinning = Just <| encode <| "{\"pinning\":[" ++ enc_cylinders pinning ++ "]}"

inc_first = Maybe.map <| Tuple.mapFirst ((+)1)
dec_first = Maybe.map <| Tuple.mapFirst ((-)1)

update_pinset pinset old new = Dict.update new inc_first
    <| Dict.update old dec_first pinset

update: Msg -> Model -> Model
update msg model =
  case msg of
    Generate ->
      {model | encoded = enc model.pinning}

    SetKP i s-> case Array.get i model.pinning of
      Just (a,b,c) -> {model | pinning = Array.set i (a,b,s) model.pinning
          , kp = update_pinset model.kp c s
          , encoded = Nothing}
      _ -> model

    SetDP i s-> case Array.get i model.pinning of
      Just (a,b,c) -> {model | pinning = Array.set i (a,s,c) model.pinning
          , dp = update_pinset model.dp b s
          , encoded = Nothing}
      _ -> model

    SetSp i s-> case Array.get i model.pinning of
      Just (_,b,c) -> {model | pinning = Array.set i (s,b,c) model.pinning
          , encoded = Nothing }
      _ -> model


nothing = {value="",  text="", enabled=True}

create_options m ls handler =
    { items= ls
    , emptyItem= Just nothing
    , onChange= handler
    }

springs = 
  [ {value="Normal", text="Normal", enabled=True}
  , {value="Short",  text="Short",  enabled=True}
  , {value="Double", text="Double", enabled=True}
  ]

create_spring m i = Dropdown.dropdown
    (create_options m springs (message SetSp i))
    [ Html.Attributes.style "width" "150px" ]
    (Maybe.map (\(s,d,k)->s) (Array.get i m.pinning))

create_springs m = div []
    [ div
      [ Html.Attributes.style "width" "100px"
      , Html.Attributes.style "float" "left"
      ]
      [text "Springs:"]
    , create_spring m 0
    , create_spring m 1
    , create_spring m 2
    , create_spring m 3
    , create_spring m 4
    ]

drivers =
  [ {value="Normal1", text="6.0 mm", enabled=True}
  , {value="Normal2", text="5.2 mm", enabled=True}
  , {value="Normal3", text="4.5mm", enabled=True}
  , {value="Serrated1", text="Serrated 5.9 mm", enabled=True}
  , {value="Serrated2", text="Serrated 5.0 mm", enabled=True}
  , {value="Spool", text="Spool 5.7 mm", enabled=True}
  , {value="HalfSpool1", text="HalfSpool 5.9 mm", enabled=True}
  , {value="HalfSpool2", text="HalfSpool 5.5 mm", enabled=True}
  , {value="TPin", text="T-Pin", enabled=True}
  , {value="TwoPart", text="Two parted", enabled=True}
  ]

message m i ms = m i <| Maybe.withDefault "" ms

create_tumbler pins pinset sel msg m i = Dropdown.dropdown
    (create_options m pins (message msg i))
    (mismatch pinset (\(_,a,_)->a) m.pinning i)
    (Maybe.map (\(s,d,k)->d) (Array.get i m.pinning))


create_drivers m = let snd (_,b,_) = b in
  div []
    [ div
        [ Html.Attributes.style "width" "100px"
        , Html.Attributes.style "float" "left"
        ]
        [text "Driver pins:"]
    , create_tumbler drivers m.dp snd SetDP m 0
    , create_tumbler drivers m.dp snd SetDP m 1
    , create_tumbler drivers m.dp snd SetDP m 2
    , create_tumbler drivers m.dp snd SetDP m 3
    , create_tumbler drivers m.dp snd SetDP m 4
    ]

keypins =
  [ {value="P1",  text="9,0 mm", enabled=True}
  , {value="P2",  text="8,6 mm", enabled=True}
  , {value="P3",  text="8,1 mm", enabled=True}
  , {value="P4",  text="7,5 mm", enabled=True}
  , {value="P5",  text="7,0 mm", enabled=True}
  , {value="P6",  text="6,5 mm", enabled=True}
  , {value="P7",  text="6,2 mm", enabled=True}
  , {value="P8",  text="5,6 mm", enabled=True}
  , {value="P9",  text="5,0 mm", enabled=True}
  , {value="P10", text="4,7 mm", enabled=True}
  , {value="P11", text="rounded", enabled=True}
  ]

mismatch pinset sel pinning i = Html.Attributes.style "width" "150px" ::
    case Array.get i pinning |> andThen (\a->Dict.get (sel a) pinset) of
        Nothing -> []
        Just (x, y) -> if x>y
            then [Html.Attributes.style "color" "red"]
            else []

create_keypins m = let trd (_,_,c) = c in 
  div []
    [ div
      [ Html.Attributes.style "width" "100px"
      , Html.Attributes.style "float" "left"
      ]
      [text "Key pins:"]
    , create_tumbler keypins m.kp trd SetKP m 0
    , create_tumbler keypins m.kp trd SetKP m 1
    , create_tumbler keypins m.kp trd SetKP m 2
    , create_tumbler keypins m.kp trd SetKP m 3
    , create_tumbler keypins m.kp trd SetKP m 4
    ]

tuple_partial_empty (a,b,c) = let ls = [a,b,c] in
    List.any String.isEmpty ls && not (List.all String.isEmpty ls)

invalid_pinning = List.any tuple_partial_empty

valid_count model = let all_in_range = List.all (\(a,b)-> a<=b) << Dict.values
    in all_in_range model.kp
    && all_in_range model.dp

invalid model = let ls = Array.toList model.pinning in 
    List.all tuple_empty ls
    || invalid_pinning ls
    || not (valid_count model)

view model =
  div [Html.Attributes.style "margin" "10px"]
    [ Html.h2 [] [text "Challenge generator"]
    , create_keypins model
    , create_drivers model
    , create_springs model
    , div [
        Html.Attributes.style "margin-top" "10px"
    ] [button [ onClick Generate, disabled <| invalid model ] [ text "generate" ]]
    , Html.input
        [ value <| withDefault "" model.encoded
        , disabled True
        , Html.Attributes.style "width" "100%"
        , Html.Attributes.style "margin-top" "10px"
        ]
        []
    ]
