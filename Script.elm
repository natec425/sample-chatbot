module Script exposing (Script, sampleScript)

import String
import Html exposing (Html, Attribute, div, span, text, input)
import Html.Events exposing (onInput, on, keyCode)
import Html.Attributes exposing (style)
import Html.App
import Json.Decode as Json
import Debug exposing (log)

type alias WelcomeMessage = String

type alias EmailAddress = String

type Keyword
    = Match String
    | Capture

type alias Response = String

type alias Branch = (Keyword, String, Script)

type Script
    = Root WelcomeMessage Script
    | Node (List Branch)
    | Leaf EmailAddress

emailMegan : Script
emailMegan = Leaf "megan@theorchardoxford.net"
emailDon : Script
emailDon = Leaf "don@theorchardoxford.net"

sampleScript : Script
sampleScript =
    Root "Welcome to the Orchard Oxford's Chatbot! Where would you like to serve?"
         (Node [ (Match "children", "Wonderful! Our Children's Minister will get in touch with you as soon as possible", emailMegan)
         , (Match "college", "Thanks so much! Our College Minister will get in touch with you as soon as possible", emailDon) ])

type alias Model =
    { script : Script
    , chatLog : List Msg
    , history : List String
    , input : String }

type Msg
    = NoOp
    | Receive String
    | Send String
    | UpdateInput String
    | SendInput
    | Email EmailAddress (List String)

initModel : Model
initModel =
    { script = sampleScript
    , chatLog = []
    , history = []
    , input = "" }

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case log "msg" msg of
        NoOp -> (model, Cmd.none)
        Receive m ->
            afterReceiving m model
        Send m ->
            ({ model | chatLog = msg :: model.chatLog }
            , Cmd.none)
        Email a h ->
            ({ model | chatLog = msg :: model.chatLog, script = initModel.script, history = [] },
             Cmd.none)
        UpdateInput s ->
            ({ model | input = s }, Cmd.none)
        SendInput ->
            update (Receive model.input) { model | input = "" }

afterReceiving : String -> Model -> (Model, Cmd Msg)
afterReceiving message model =
    case log "script" model.script of
        Root welcome after ->
            update (Send (welcome ++ optionsMessage after))
                { model | script = after
                , history = model.history
                , chatLog = Receive message :: model.chatLog }
        Node branches ->
            case takenBranch message branches of
                Just (k, r, Leaf address) ->
                    { model | script = initModel.script
                            , history = message :: model.history
                            , chatLog = (Receive message) :: model.chatLog }
                    |> update (Send r)
                    |> (\(m, c) -> update (Email address (List.reverse m.history)) m)
                Just (k, r, s) ->
                    update (Send (r ++ optionsMessage s))
                           {model | script = s
                            , history = message :: model.history
                            , chatLog = Receive message :: model.chatLog }
                Nothing ->
                    update (Send  ("I'm sorry, I couldn't understand your response. Please try one of the following options: " ++ optionsMessage model.script))
                            { model | chatLog = (Receive message) :: model.chatLog }
        Leaf address ->
            update (Email address model.history) model

takenBranch : String -> List (Keyword, String, Script) -> Maybe (Keyword, String, Script)
takenBranch m branches =
    branches
    |> List.filter (matches m)
    |> List.head

matches : String -> Branch -> Bool
matches m (k, _, _) =
    case k of
        Match m' -> m == m'
        Capture -> True

keywordToOption : Keyword -> String
keywordToOption k =
    case k of
        Match s -> s
        Capture -> "Capture"

optionsMessage : Script -> String
optionsMessage s =
    case s of
        Node branches ->
            branches
            |> List.map (\(a, _, _) -> a)
            |> List.map keywordToOption
            |> String.join ", "
        _ -> ""

-- VIEW

view : Model -> Html Msg
view model =
    div [ baseStyle ]
        [ viewChatLog model.chatLog
        , viewInput model.input ]

viewChatLog : List Msg -> Html Msg
viewChatLog msgs =
    div [] (List.map viewMsg (List.reverse msgs))

viewMsg : Msg -> Html Msg
viewMsg m =
    case m of
        Send m -> div [ messageStyle, sentStyle ] [ text <| "ChatBot: " ++ m ]
        Receive m -> div [messageStyle, receivedStyle ] [ text <| "User: " ++ m ]
        Email a h -> div [messageStyle, emailStyle] [ text <| "*** EMAILING " ++ a ++ " *** with the phone number and user responses: " ++ String.join ", " h ]
        _ -> text ""

viewInput : String -> Html Msg
viewInput s =
    input [ onInput UpdateInput
          , onEnter SendInput
          , inputStyle ]
          [ text s ]

onEnter : Msg -> Attribute Msg
onEnter msg =
  let
    tagger code =
      if log "code" code == 13 then msg else NoOp
  in
    on "keydown" (Json.map tagger keyCode)

messageStyle : Attribute Msg
messageStyle =
    style [
        ("padding-top", ".75em")
        , ("padding-bottom", ".75em")
        , ("padding-left", "2.5%")
        , ("padding-right", "2.5%")
        , ("width", "85%")
        , ("margin-top", "1%")
        , ("margin-bottom", "1%")
        , ("border-radius", "10px")
    ]

sentStyle : Attribute Msg
sentStyle =
    style [
        ("text-align", "right")
        , ("margin-left", "10%")
        , ("margin-right", "0%")
        , ("background-color", "#a1b2a1")
    ]

receivedStyle : Attribute Msg
receivedStyle =
    style [
        ("text-align", "left")
        , ("margin-left", "0%")
        , ("margin-right", "10%")
        , ("background-color", "#caceca")
    ]

inputStyle : Attribute Msg
inputStyle =
    style [
        ("text-align", "center")
        , ("width", "100%")
        , ("height", "2em")
        , ("font-size", "1em")
    ]

baseStyle : Attribute Msg
baseStyle =
    style [
        ("font-size", "2em")
    ]

emailStyle : Attribute Msg
emailStyle =
    style [
        ("background-color", "#ff6d6d")
        , ("width", "75%")
        , ("margin-left", "auto")
        , ("margin-right", "auto")
    ]
-- MAIN

main = Html.App.program { init = (initModel, Cmd.none)
                        , view = view
                        , update = update
                        , subscriptions = always Sub.none }