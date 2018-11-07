-- Import dependencies at the top level of the file
import Browser
import Http
import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Json.Decode as Decode exposing (..)
import Json.Encode as Encode exposing (..)

-- Main
-- main : Program Never Model Msg -- type annotation
-- main = 
--   Html.program 
--     { init = init
--     , update = update 
--     , subscriptions = \_ -> Sub.none -- Anonymous function requiring no arguments
--     , view = view
--     }
main =
  Browser.element
    { init = init
    , update = update
    , subscriptions = \_ -> Sub.none -- Anonymous function requiring no arguments
    -- , subscriptions = subscriptions
    , view = view
    }

-- -- SUBSCRIPTIONS
-- subscriptions : Model -> Sub Msg
-- subscriptions model =
--   Sub.none

----------------
-- MODEL --
----------------

type alias Model =
  { username: String,
    password: String,
    token: String,
    quote : String, -- Just have the quote before authentication
    errorMsg: String
  }

init : () -> (Model, Cmd Msg)
init _ =
  ( Model "" "" "" "" ""
  -- , Cmd.none
  , fetchRandomQuoteCmd
  )

----------------
-- UPDATE --
----------------
{-
  * API routes
  * GET and POST
  * Encode request body 
  * Decode responses
  * Messages
  * Update case
-}
-- Defining the API request urls
api : String
api = 
  "http://localhost:3001/"

randomQuoteUrl : String
randomQuoteUrl =
  api ++ "api/random-quote"

registerUrl : String
registerUrl =
    api ++ "users"

loginUrl : String
loginUrl =
    api ++ "sessions/create"

-- Encode user to construct POST request body (for Register and Log In)
{-
An Elm record is not the same as a JavaScript object so we 
need to encode the applicable properties of our model before 
we can send them with the HTTP request. The userEncoder function 
utilizes Json.Encode to take the model and return the encoded value.
-}
userEncoder : Model -> Encode.Value
userEncoder model = 
    Encode.object 
        [ ("username", Encode.string model.username)
        , ("password", Encode.string model.password) 
        ]          

-- POST register / login request
{-
Create an effect function authUser (for "authenticate a user"). 
authUser takes model as an argument and a string as an argument 
and returns a request that succeeds with a string.
-}
authUser : Model -> String -> Http.Request String
authUser model apiUrl =
    let
        -- body = model <| userEncoder <| Http.jsonBody
        -- body = model (userEncoder (Http.jsonBody))
        body =
            model
                |> userEncoder
                |> Http.jsonBody
    in
        Http.post apiUrl body tokenDecoder

authUserCmd : Model -> String -> Cmd Msg
authUserCmd model apiUrl = 
  Http.send GetTokenCompleted (authUser model apiUrl)

getTokenCompleted : Model -> Result Http.Error String -> ( Model, Cmd Msg )
getTokenCompleted model result = 
  case result of
      Ok newToken ->
        ( { model | token = newToken, password = "", errorMsg = "" 
          } 
          |> Debug.log "got new token", Cmd.none 
        )
  
      Err error ->
        ( { model | errorMsg = (Debug.toString error) }, Cmd.none )

tokenDecoder : Decoder String
tokenDecoder =
    Decode.field "access_token" Decode.string
          
-- Get a random quote (unauthenticated)

fetchRandomQuote : Http.Request String
fetchRandomQuote = 
  Http.getString randomQuoteUrl

fetchRandomQuoteCmd : Cmd Msg
fetchRandomQuoteCmd =
  Http.send FetchRandomQuoteCompleted fetchRandomQuote

fetchRandomQuoteCompleted : Model -> Result Http.Error String -> ( Model, Cmd Msg )
fetchRandomQuoteCompleted model result = 
  case result of
      Ok newQuote ->
        ( { model | quote = newQuote }, Cmd.none )
  
      Err _ ->
        ( model, Cmd.none )

-- Messages 
type Msg 
  = GetQuote -- Custom/Union type
  | FetchRandomQuoteCompleted (Result Http.Error String)
  | SetUsername String
  | SetPassword String
  | ClickRegisterUser
  | ClickLogin
  | LogOut
  | GetTokenCompleted (Result Http.Error String)

-- Update
update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    GetQuote ->
      ( model
      , fetchRandomQuoteCmd
      )
      -- ( { model | quote = model.quote ++ "A quote! "}, Cmd.none)

    FetchRandomQuoteCompleted result -> 
      fetchRandomQuoteCompleted model result

    -- Send Form field values to update the model
    SetUsername username ->
      ( { model | username = username }, Cmd.none )

    SetPassword password ->
      ( { model | password = password }, Cmd.none )

    -- onClick for our "Register" button. 
    -- It runs the authUserCmd command and passes the model and 
    -- the API route for new user creation.
    ClickRegisterUser ->
      ( model, authUserCmd model registerUrl )
    
    --response handling function for the authUser request. 
    -- Its argument is the token string result. 
    GetTokenCompleted result ->
      getTokenCompleted model result

    -- Runs the authUserCmd command with the appropriate arguments. 
    ClickLogin ->
      ( model, authUserCmd model loginUrl)

    -- resets authentication-related data in the model record to 
    -- empty strings. We don't need to reset the password or 
    -- errorMsg because we already did so when we successfully 
    -- retrieved a token in GetTokenCompleted.
    LogOut -> 
      ( { model | username = "", token = ""}, Cmd.none )

----------------
-- VIEW
----------------

-- View takes a model as an argument and returns HTML with a message
view : Model -> Html Msg
view model =
  let
    -- Is the user logged in?
    loggedIn : Bool
    loggedIn =
        -- In a real-world application, token verification might 
        -- be performed in a route callback to ensure proper UI access
        if String.length model.token > 0 then
            True
        else
            False

    -- If the user is logged in, show a greeting; 
    -- if logged out, show the login/register form
    authBoxView =
        let
            -- If there is an error on authentication, show the error alert
            showError : String
            showError =
                if String.isEmpty model.errorMsg then
                    "hidden"
                else
                    ""

            -- Greet a logged in user by username
            greeting : String
            greeting =
                "Hello, " ++ model.username ++ "!"
        in
            if loggedIn then
                div [ id "greeting" ]
                    [ h3 [ class "text-center" ] [ text greeting ]
                    , p [ class "text-center" ] [ 
                      text "You have super-secret access to protected quotes." ]
                    , p [ class "text-center" ] [
                      button [ class "btn btn-danger", onClick LogOut ] [
                        text "Log Out"
                      ]
                    ]
                    ]
            else
                div [ id "form" ]
                    [ h2 [ class "text-center" ] [ text "Log In or Register" ]
                    , p [ class "help-block" ] [ 
                      text """If you already have an account, please Log In. 
                      Otherwise, enter your desired username and password and Register.""" ]
                    , div [ class showError ]
                        [ div [ class "alert alert-danger" ] [ text model.errorMsg ]
                        ]
                    , div [ class "form-group row" ]
                        [ div [ class "col-md-offset-2 col-md-8" ]
                            [ label [ for "username" ] [ text "Username:" ]
                            , input [ 
                              id "username"
                              , type_ "text"
                              , class "form-control"
                              , Html.Attributes.value model.username
                              , onInput SetUsername ] []
                            ]
                        ]
                    , div [ class "form-group row" ]
                        [ div [ class "col-md-offset-2 col-md-8" ]
                            [ label [ for "password" ] [ text "Password:" ]
                            , input [ 
                              id "password"
                              , type_ "password"
                              , class "form-control"
                              , Html.Attributes.value model.password
                              , onInput SetPassword ] []
                            ]
                        ]
                    , div [ class "text-center" ] [
                        button [ class "btn btn-primary", onClick ClickLogin ] [
                          text "Log In" ]
                        , button [ class "btn btn-link" , onClick ClickRegisterUser ] [
                          text "Register" ]
                        ]
                    ]
  in
    div [ class "container" ] [
      h2 [ class "text-center" ] [ text "Chuck Norris Quotes" ]
      , p [ class "text-center" ] [
        button [ class "btn btn-success", onClick GetQuote ] [ 
          text "Grab a quote!"  
        ]
      ]
        -- Blockquote with quote
        , blockquote [ class "help-block" ] [
            p [] [text model.quote]
        ]
        , div [ class "jumbotron text-left" ] [ 
            -- Login/Register form or user greeting
            authBoxView
        ]
    ]
