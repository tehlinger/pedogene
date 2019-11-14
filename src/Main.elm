module Main exposing (main)

import Browser
import Select
import Html exposing (Html, text, div, h1, img, input,br)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick,onInput)


type alias Model =
    { parents : (Individu,Individu)
    , content : String}

type Allele
    = Bleu
    | Vert
    | Marron

type alias Individu =
    { mater : Allele
    , pater : Allele
    }

alleleDominant : Allele -> Allele -> Allele
alleleDominant a1 a2 =
    case (a1, a2) of
        (nimporte, Bleu) ->
            nimporte

        (_, Marron) ->
            Marron

        (allele, Vert) ->
            case allele of
                Bleu ->
                    Vert

                Vert ->
                    Vert

                Marron ->
                    Marron

calcEnfant : Individu -> Individu -> Allele
calcEnfant pere mere =
    alleleDominant pere.pater mere.pater

initialModel : Model
initialModel =
    { parents = ( Individu Bleu Marron , Individu Bleu Vert )
    ,content = ""}


type Msg = ChangeMom String
  | ChangeDad String


update : Msg -> Model -> Model
update msg model =
    case msg of
      ChangeMom newContent -> {model | parents = ( (indFromString newContent) , Individu Bleu Vert ), content = newContent }
      ChangeDad newContent -> { model | content = newContent }

indFromString : String -> Individu
indFromString s = (Individu Bleu Bleu)

alleleToString : Allele -> String
alleleToString allelle =
    case allelle of
        Bleu -> "B"
        Vert -> "V"
        Marron -> "M"

stringToAllele : String -> (Allele, Allele)
stringToAllele s =
    case s of
        _ -> (Vert,Vert)

afficherIndividu : Individu -> Html Msg
afficherIndividu individu =
    div [] [ text (alleleToString individu.mater)
           , text (alleleToString individu.pater)
           ]


view : Model -> Html Msg
view model =
    case model.parents of
        (p,m) ->
            div []
            [
              div [][text "Allèles mère : "
            , input [ placeholder "BB,BM,VV...", value model.content, onInput ChangeMom ] []]
            , br [][]
            , div [][text "Allèles père - OSEF : "
            ,input [ placeholder "BB,BM,VV...", value model.content, onInput ChangeMom ] []]
            , afficherIndividu p
            , afficherIndividu m
            , text <| "= " ++ (alleleToString <| calcEnfant p m)
            , div [] [text model.content]
            ]

main : Program () Model Msg
main =
    Browser.sandbox
        { init = initialModel
        , view = view
        , update = update
        }
