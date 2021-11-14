module Main exposing (..)


import Browser
import Browser.Events
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Json.Decode as D exposing (Decoder, int, float, string)
import Json.Decode.Pipeline as Pipeline exposing (requiredAt, optionalAt, custom)
import Http




main =
  Browser.element
    { init = init 
    , update = update
    , view = view
    , subscriptions = subscriptions
    }




-- MODEL


type alias Forecast =
  { city : String
  , description : String
  , icon : String
  , temp : Float
  , feels_like : Float
  , wind_speed : Float
  , wind_gust : Float
  , cloudiness : Int
  , humidity : Int
  , pressure : Int
  }


type Problem
  = HttpError
  | NoUserLocation


type Request
  = Loading
  | Failure Problem
  | Success Forecast


type alias Model =
  { city : String
  , request : Request
  }


-- Init function gets the object 
-- with the user's coordinates from JS

init : D.Value -> ( Model, Cmd Msg )
init userLocation =
  case D.decodeValue coordsDecoder userLocation of 
    Ok coords ->
      ( { city = ""
        , request = Loading
        }
  
      , Http.get
          { url = 
            "https://api.openweathermap.org/data/2.5/weather?" ++ 
            "lat=" ++ (String.fromFloat coords.lat) ++
            "&lon=" ++ (String.fromFloat coords.lon) ++
            "&appid=8c119af7e118aa113ea04c2ad8d834cf&units=metric&lang=ru" 
          , expect = Http.expectJson GotForecast forecastDecoder
          }
      )

    Err _ ->
      ( { city = ""
        , request = Failure NoUserLocation
        }
  
      , Cmd.none
      )




-- JSON


type alias Coords =
  { lat : Float
  , lon : Float 
  }


coordsDecoder : Decoder Coords
coordsDecoder =
  D.map2 Coords
    (D.field "lat" float)
    (D.field "lon" float)


forecastDecoder : Decoder Forecast
forecastDecoder =
  D.succeed Forecast
    |> Pipeline.required "name" string
    |> custom (D.field "weather" (D.index 0 (D.field "description" string)))
    |> custom (D.field "weather" (D.index 0 (D.field "icon" string)))
    |> requiredAt ["main", "temp"] float
    |> requiredAt ["main", "feels_like"] float
    |> requiredAt ["wind", "speed"] float
    -- fallback if wind gust is not present
    |> optionalAt ["wind", "gust"] float -1
    |> requiredAt ["clouds", "all"] int
    |> requiredAt ["main", "humidity"] int
    |> requiredAt ["main", "pressure"] int




-- UPDATE


type Msg
  = GotForecast (Result Http.Error Forecast)
  | RequestViaCity
  | InputCity String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    GotForecast result ->
      case result of
        Ok forecast ->
          ( { model | request = Success forecast, city = "" }
          , Cmd.none
          )

        Err _ ->
          ( { model | request = Failure HttpError, city = "" }
          , Cmd.none
          )

    
    RequestViaCity ->
      ( { model | request = Loading }
      , Http.get
          { url =
          -- Remove spaces on both sides of the string 
          -- when making an HTTP request
            "https://api.openweathermap.org/data/2.5/weather?" ++ 
            "q=" ++ (String.trim model.city) ++ 
            "&appid=8c119af7e118aa113ea04c2ad8d834cf&units=metric&lang=ru" 
          , expect = Http.expectJson GotForecast forecastDecoder
          }
      )


    InputCity city ->
    -- Remove spaces on the left of a string 
    -- to cannot write with only spaces
      ( { model | city = String.trimLeft city }
      , Cmd.none
      )




-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  if not (String.isEmpty model.city) then
    Browser.Events.onKeyDown eventDecoder
  else
    Sub.none


eventDecoder : Decoder Msg
eventDecoder =
  keyDecoder
    |> D.andThen (\key -> 
      if key == 13 then 
        D.succeed RequestViaCity

      else
        D.fail ""
    )


keyDecoder : Decoder Int
keyDecoder =
  D.field "keyCode" int




-- VIEW


view : Model -> Html Msg
view model =
  main_
    [ style "margin" "auto"
    , style "margin-top" "30px"
    , style "width" "360px"
    ]
    [ div
        [ style "margin-bottom" "20px"
        , style "text-align" "center"
        ]
        [ viewInput "Введите город" InputCity model.city
        ]
    , div 
        [ style "margin-bottom" "38px"
        , style "height" "38px"
        , style "text-align" "center"
        ] 
        [ btn RequestViaCity model.city 
        ]
    , case model.request of 
        Loading ->
          alert "Загрузка..."

        Failure problem ->
          case problem of
            NoUserLocation ->
              alert "Не удалось определить ваше местоположение"

            HttpError ->
              alert "Ошибка. Возможные причины: Вы неправильно ввели название города, нет доступа к интернету, или сервер не отвечает"

        Success forecast ->
          article []
            [ viewCity forecast.city
            , generalForecast forecast
            , weatherParams forecast
            ]
    ]


weatherParams : Forecast -> Html Msg
weatherParams forecast =
  div
    [ style "padding" "0 0px 0 65px"
    , style "height" "138px"
    ]
    [ div 
        [ style "width" "135px"
        , style "height" "64px"
        , style "margin" "0 0 10px 10px"
        , style "float" "left"
        , title "Скорость и порыв ветра"
        ]
        [ viewImage "Ветер" "img/wind.png"
        , div
            [ style "width" "93px"
            , style "height" "64px"
            , style "float" "left"
            , style "margin-left" "10px"
            , style "padding-top" "5px"
            ]
            [ div
                [ style "font" "15px 'Tahoma'"
                , style "margin-bottom" "4px"
                ]
                [ text (
                    (String.fromInt (Basics.round forecast.wind_speed))
                    ++ " м/с"
                  )
                ]
            , div
                [ style "font" "12px 'Tahoma'"
                ]
                [ text (
                    if forecast.wind_gust > -1 then
                      "до " ++
                      (String.fromInt (Basics.round forecast.wind_gust)) 
                      ++ " м/с"
                    else
                      ""
                  )
                ]
            ]
        ]
    , div 
        [ style "width" "135px"
        , style "height" "64px"
        , style "margin" "0 0 10px 0"
        , style "float" "left"
        , title "Облачность"
        ]
        [ viewImage "Облака" "img/cloud.png"
        , param ((String.fromInt forecast.cloudiness) ++ "%")
        ]
    , div 
        [ style "width" "135px"
        , style "height" "64px"
        , style "margin" "0 0 10px 10px"
        , style "float" "left"
        , title "Влажность"
        ]
        [ viewImage "Влажность" "img/humidity.png"
        , param ((String.fromInt forecast.humidity) ++ "%")
        ]
    , div 
        [ style "width" "135px"
        , style "height" "64px"
        , style "margin" "0 0 10px 0"
        , style "float" "left"
        , title "Давление"
        ]
        [ viewImage "Давление" "img/meter.png" 
        , param ((String.fromInt forecast.pressure) ++ " hPa")
        ]
    ]


param : String -> Html Msg
param content =
  div
    [ style "width" "93px"
    , style "height" "64px"
    , style "float" "left"
    , style "margin-left" "10px"
    , style "padding-top" "5px"
    , style "font" "15px 'Tahoma'"
    ]
    [ text content ]


viewImage : String -> String -> Html Msg
viewImage alt_ src_ =
  div
    [ style "width" "32px"
    , style "height" "64px"
    , style "float" "left"
    ]
    [ img [ alt alt_, src src_ ] []
    ]


generalForecast : Forecast -> Html Msg
generalForecast forecast =
  div 
    [ style "margin-bottom" "60px" 
    , style "height" "100px"
    , style "padding" "0 20px"
    ]
    [ div 
        [ style "width" "100px"
        , style "height" "75px"
        , style "padding-top" "25px"
        , style "float" "left"
        ]
        [ div
            [ style "margin-bottom" "5px"
            , style "text-align" "right"
            , style "font" "20px 'Tahoma'"
            ]
            [ text (getFormatTemp forecast.temp)
            ]
        , div 
            [ style "text-align" "right"
            , style "font" "14px 'Tahoma'"
            ] 
            [ text "Ощущается как "
            , text (getFormatTemp forecast.feels_like) 
            ]
        ]
    , div 
        [ style "width" "100px"
        , style "height" "100px"
        , style "margin" "0 5px 0 5px"
        , style "float" "left"
        ]
        [ img 
            [ alt "Иконка погоды"
            , src (
                "http://openweathermap.org/img/wn/" ++
                forecast.icon ++ "@2x.png"
              )
            ]
            []
        ]
    , div 
        [ style "width" "100px"
        , style "height" "60px"
        , style "padding-top" "40px"
        , style "float" "left"
        , style "text-align" "left"
        , style "font" "18px 'Tahoma'" 
        ]
        [ text forecast.description
        ]
    ]


getFormatTemp : Float -> String
getFormatTemp temp =
  if temp > 0 then
    "+" ++ 
    (String.fromInt (Basics.round temp)) ++
    "℃"
  else
    String.fromInt (Basics.round temp) ++
    "℃"


viewCity : String -> Html Msg
viewCity city =
  div
    [ style "margin-bottom" "40px" 
    , style "text-align" "center"
    ]
    [ img [ alt "Местоположение", src "img/location.png" ] [] 
    , span 
        [ style "margin-left" "5px"
        , style "font" "16px 'Tahoma'" 
        ] 
        [ text city ]
    ]


alert : String -> Html Msg
alert content =
  p 
    [ style "padding" "0 10px 0 10px"
    , style "text-align" "center"
    , style "font" "16px 'Tahoma'" 
    ] 
    [ text content ]    


viewInput : String -> (String -> Msg) -> String -> Html Msg
viewInput pl msg v =
  input
    [ type_ "text"
    , autofocus True
    , placeholder pl
    , onInput msg
    , value v
    , style "outline" "none"
    , style "border" "none"
    , style "text-align" "center"
    , style "font" "16px 'Tahoma'"
    ]
    []


btn : Msg -> String -> Html Msg
btn msg city =
  if not (String.isEmpty city) then
    button 
      [ onClick msg
      , style "padding" "10px"
      , style "outline" "none"
      , style "border" "none"
      , style "border-radius" "5px"
      , style "background" "#e8e8e8"
      , style "font" "15px 'Tahoma'"
      , style "cursor" "pointer"
      ] 
      [ text "Найти" 
      ]
  else
    text ""




