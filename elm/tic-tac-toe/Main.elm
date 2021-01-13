-- Tic-tac-toe by Taylor Allred for CS 330
--


module Main exposing (Model, Msg, init, main, update, view)

import Browser
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)



-- MAIN


main =
    Browser.sandbox { init = init, update = update, view = view }



-- MODEL

type alias Square =
    { val : String
    , pos : Int
    }

type alias Model = -- Your code here
    { isXTurn : Bool
    , board : List Square
    }

init : Model
init = -- Your code here
    Model True (List.map (\index -> (Square "" index)) (List.range 0 8)) -- isXTurn and list of unclaimed squares

-- UPDATE

type Msg = -- Your code here
    XSquare Int
    | OSquare Int
    | Reset

update : Msg -> Model -> Model
update msg model = -- Your code here
    case msg of
        XSquare pos ->
            Model (not model.isXTurn) (List.indexedMap (\index sq -> if index == pos && sq.val == "" then (Square "X" index) else sq) model.board)
        OSquare pos ->
            Model (not model.isXTurn) (List.indexedMap (\index sq -> if index == pos && sq.val == "" then (Square "O" index) else sq) model.board)
        Reset ->
            init

-- VIEW

makeListofHtml : List Square -> Bool -> List (Html Msg)
makeListofHtml squares isXTurn =
    List.map (\sq -> if sq.val == "X" then div [] [text "X"]
                        else if sq.val == "O" then div [] [text "O"]
                            else button [onClick (if isXTurn then (XSquare sq.pos) else (OSquare sq.pos))] [text ""]) squares

view : Model -> Html Msg
view model =
    div [ class "content" ]
        [ div [ class "grid" ]
            (makeListofHtml model.board model.isXTurn)
        , div [ class "turn" ] [ text (if model.isXTurn then
                                        "X\'s turn"
                                        else
                                            "O\'s turn") ]
        , div []
            [ button [ onClick (Reset) ] [ text "Reset" ]
            ]
        ]
