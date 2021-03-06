import Html exposing (..)
import Html.Events exposing (onClick)
import Html.Attributes exposing (..)
import Math exposing(..)
import Data exposing(..)

type alias Model =
  {
    crossSection : Int,
    metal : MetalType,
    length : Int,
    ampers : Int,
    power : Int
  }
    
model : Model
model =
  Model 0 Cu 0 0 0

init : ( Model, Cmd Msg )
init = 
  ( model, Cmd.none )


main : Program Never Model Msg
main =
  Html.program { init = init, view = view, update = update, subscriptions = subscriptions }


type Msg = Increment | Decrement

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )

view : Model -> Html Msg
view model =
  Html.form []
  [
    label []
      [ select [] [ option [] [ text "Алюминий"], option [] [ text "Медь"] ]
      , text "Тип кабеля"
      ]
    , label []
      [ input [ type_ "number"] []
      , text "Длина кабеля, м" ]
  ]

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none