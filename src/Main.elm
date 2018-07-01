port module Main exposing (main)


import AnimationFrame as Anim
import Css
import Html
import Html.Styled as Styled
import Html.Styled.Attributes as Attr
import Html.Styled.Events as Events
import Json.Decode as JsonD
-- import Json.Decode.Pipeline as Pipe
import Keyboard
import Keyboard.Extra as Keys
import Random as Rand
import Task
import Time


---- MODEL ----


type alias Vector2 =
    { x : Float
    , y : Float
    }


type alias GameObject =
    { position : Vector2
    , velocity : Vector2
    }


type GameState
    = NotYetStarted
    | Running
    | GameOver
    | Paused


type alias HighScore =
    { player : String
    , score : Int
    }


type alias Model =
    { player : GameObject
    , fallingThings : List GameObject
    , randSeed : Rand.Seed
    , gameState : GameState
    , score : Int
    , moveDirection : Float
    , highScores : List HighScore
    , playerName : String
    }


init : ( Model, Cmd Msg )
init =
    ( { player = initialPLayer
      , fallingThings = []
      , randSeed = Rand.initialSeed 0
      , gameState = NotYetStarted
      , score = 0
      , moveDirection = 0
      , highScores = []
      , playerName = ""
      }
    , Cmd.batch [ Task.perform SetInitialSeed Time.now
                , getScores ()
                ]
    )


initialPLayer : GameObject
initialPLayer = GameObject (Vector2 45 0) (Vector2 20 0)


newGameState : Model -> Model
newGameState model =
    { model
    | player = initialPLayer
    , fallingThings = []
    , score = 0
    , moveDirection = 0
    , gameState = Running
    }


unitSize : Float
unitSize =
    10


minX : Float
minX =
    0


maxX : Float
maxX =
    90


initialY : Float
initialY =
    100

scoreMultiplier : Int
scoreMultiplier =
    2


endY : Float
endY =
    -unitSize


minPlayerPosition : Vector2
minPlayerPosition =
    Vector2 minX 0

maxPlayerPosition : Vector2
maxPlayerPosition =
    Vector2 maxX 0

cssBlack : Css.Color
cssBlack =
    Css.rgb 40 40 40


cssFadedBlack : Css.Color
cssFadedBlack =
    Css.rgba 40 40 40 0.75


cssWhite : Css.Color
cssWhite =
    Css.rgb 250 250 250


---- UPDATE ----


type Msg
    = NoOp
    | Move Float
    | Spawn Time.Time
    | SetSeed Rand.Seed
    | SetInitialSeed Time.Time
    | Tick Time.Time
    | StartGame
    | EndGame
    | PauseGame
    | LoseFocus String
    | GetHighScores
    | ReceiveHighScores (List HighScore)
    | SaveHighScore
    | SetPlayerName String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )
        Move direction ->
            ( { model | moveDirection = direction }, Cmd.none )
        SetSeed seed ->
            ( { model | randSeed = seed }, Cmd.none)
        SetInitialSeed time ->
            ( { model | randSeed = Rand.initialSeed (floor time) }
            , Cmd.none
            )
        Spawn seed ->
            case model.gameState of
                Running ->
                    let
                        ( xPos, seed1 ) = Rand.step (Rand.float minX maxX) model.randSeed
                        ( yVel, seed2 ) = Rand.step (Rand.float 4 13) seed1
                        newFallingThings = [ GameObject (Vector2 xPos initialY) (Vector2 0 -yVel) ] ++ model.fallingThings
                    in
                        ( { model | fallingThings = newFallingThings, randSeed = seed2 }
                        , Cmd.none
                        )
                _ ->
                    ( model, Cmd.none)
        Tick deltaTime ->
            case model.gameState of
                Running ->
                    let
                        -- Update Falling Things
                        moveByDeltaTime = moveGameObject deltaTime
                        movedFallingThings = List.map moveByDeltaTime model.fallingThings
                        (filteredFallingThings, objectsDodged) = List.partition (\ft -> ft.position.y > endY) movedFallingThings
                        -- Update Score
                        newScore = List.foldr (\go total -> total + (round -go.velocity.y) * scoreMultiplier) model.score objectsDodged
                        -- Update Player
                        newPosition = moveVector2 deltaTime model.player.position (multVector2 model.player.velocity model.moveDirection)
                        newPlayer = if newPosition.x < minX then
                                        GameObject minPlayerPosition model.player.velocity
                                    else if newPosition.x > maxX then
                                        GameObject maxPlayerPosition model.player.velocity
                                    else
                                        GameObject newPosition model.player.velocity
                        -- Update Game State
                        playerHit = checkCollision newPlayer
                        newGameState = if List.any playerHit filteredFallingThings then
                                            GameOver
                                        else
                                            model.gameState
                    in
                        case newGameState of
                            Running ->
                                ( { model
                                  | fallingThings = filteredFallingThings
                                  , score = newScore
                                  , player = newPlayer
                                  }
                                , Cmd.none
                                )
                            _ ->
                                ( { model | gameState = newGameState }, Cmd.none )
                _ ->
                    ( model, Cmd.none)
        StartGame ->
            ( newGameState model, Cmd.none )
        EndGame ->
            ( { model | gameState = GameOver }, Cmd.none )
        PauseGame ->
            case model.gameState of
                Running ->
                    ( { model | gameState = Paused }, Cmd.none )
                Paused ->
                    ( { model | gameState = Running }, Cmd.none )
                _ ->
                    ( model, Cmd.none )
        LoseFocus _ ->
            case model.gameState of
                Running ->
                    ( { model | gameState = Paused }, Cmd.none )
                _ ->
                    ( model, Cmd.none )
        GetHighScores ->
            ( model, getScores () )
        SaveHighScore ->
            ( { model | gameState = NotYetStarted, playerName = "" }
            , saveScore (model.playerName, model.score)
            )
        ReceiveHighScores scores ->
            ( { model | highScores = scores }, Cmd.none )
        SetPlayerName name ->
            ( { model | playerName = name }, Cmd.none )


checkCollision : GameObject -> GameObject -> Bool
checkCollision player fallingThing =
    let
        playerLeft = player.position.x
        playerRight = playerLeft + unitSize
        playerBottom = player.position.y
        playerTop = playerBottom + unitSize

        ftLeft = fallingThing.position.x
        ftRight = ftLeft + unitSize
        ftBottom = fallingThing.position.y
        ftTop = playerBottom + unitSize
    in
        not (playerLeft > ftRight ||
            playerRight < ftLeft ||
            playerTop < ftBottom ||
            playerBottom > ftTop)


moveGameObject : Float -> GameObject -> GameObject
moveGameObject dt go =
    GameObject (moveVector2 dt go.position go.velocity) go.velocity


moveVector2 dt pos vel =
    addVector2 pos (multVector2 vel dt)


addVector2 : Vector2 -> Vector2 -> Vector2
addVector2 v1 v2 =
    Vector2 (v1.x + v2.x) (v1.y + v2.y)


multVector2 : Vector2 -> Float -> Vector2
multVector2 { x, y } mult =
    Vector2 (x * mult) (y * mult)


---- VIEW ----


view : Model -> Styled.Html Msg
view { player, fallingThings, score, gameState, playerName, highScores } =
    Styled.div []
               ( [ playerObject player
                 , scoreboard score highScores
                 , submitScoreView gameState playerName
                 , startButton gameState
                 ] ++
                 (List.map (\fallingThing -> enemyObject fallingThing) fallingThings)
               )


submitScoreView : GameState -> String -> Styled.Html Msg
submitScoreView gameState playerName =
    case gameState of
        GameOver ->
            Styled.div [ Attr.css [ Css.position Css.absolute
                                  , Css.top (Css.vh 10)
                                  , Css.left (Css.vw 50)
                                  , Css.transform (Css.translateX (Css.pct -50))
                                  , Css.displayFlex
                                  , Css.flexDirection Css.column
                                  , Css.backgroundColor cssFadedBlack
                                  , Css.padding2 (Css.vh 2) (Css.vw 2)
                                  , Css.zIndex (Css.int 6)
                                  ]
                       ]
                       [ Styled.input [ Attr.css [ Css.marginBottom (Css.vh 3)
                                                 , Css.fontSize (Css.vw 3)
                                                 , Css.textAlign Css.center
                                                 ]
                                      , Events.onInput SetPlayerName
                                      ] []
                       , Styled.button [ Attr.css [ Css.fontSize (Css.vw 3)
                                                  , Css.cursor Css.pointer
                                                  , Css.backgroundColor (Css.rgb 86 241 208)
                                                  , Css.borderStyle Css.none
                                                  , Css.color cssWhite
                                                  , Css.padding2 (Css.vh 2) (Css.vw 3)
                                                  , Css.cursor Css.pointer
                                                  , Css.hover buttonHover
                                                  , Css.focus buttonHover
                                                  ]
                                       , Events.onClick SaveHighScore
                                       ]
                                       [ Styled.text "Submit Score" ]
                       ]
        _ ->
            Styled.text ""



scoreboard : Int -> List HighScore -> Styled.Html Msg
scoreboard score highScores =
    Styled.div [ Attr.css [ Css.fontSize (Css.vw 3)
                          , Css.backgroundColor cssFadedBlack
                          , Css.color cssWhite
                          , Css.displayFlex
                          , Css.padding2 (Css.vh 0.5) (Css.vw 0)
                          , Css.zIndex (Css.int 5)
                          , Css.position Css.absolute
                          , Css.width (Css.vw 100)
                          ]
               ]
               [ Styled.span [ Attr.css [ Css.flex (Css.num 1)
                                        , Css.textAlign Css.left
                                        , Css.paddingLeft (Css.vw 1)
                                        ]
                             ]
                             [ Styled.text <| "Score: " ++ (toString score) ]
               , Styled.div [ Attr.css [ Css.marginRight (Css.vw 1)
                                       , Css.cursor Css.pointer
                                       ]
                            , Attr.class "high-score-list-button"
                            ]
                            [ Styled.text "View Scores"
                            , highScoreList highScores
                            ]
               ]


highScoreList : List HighScore -> Styled.Html Msg
highScoreList highScores =
    Styled.ul [ Attr.css [ Css.position Css.absolute
                         , Css.right (Css.vw 0)
                         , Css.top (Css.vw 4.6)
                         , Css.backgroundColor cssFadedBlack
                         , Css.zIndex (Css.int 10)
                         , Css.listStyle Css.none
                         , Css.margin (Css.rem 0)
                         , Css.padding (Css.rem 0)
                         ]
              ]
              ( ( Styled.li [ Attr.css [ Css.padding3 (Css.vh 1) (Css.vw 1) (Css.vh 0)
                                       , Css.textDecoration Css.underline
                                       ]
                            ]
                            [ Styled.text "High Scores" ] )
              :: List.map highScoreItem highScores
              )


highScoreItem : HighScore -> Styled.Html Msg
highScoreItem { player, score } =
    Styled.li [ Attr.css [ Css.padding2 (Css.vh 1) (Css.vw 1)
                         ]
              ]
              [ Styled.text (player ++ ": " ++ (toString score)) ]


startButton : GameState -> Styled.Html Msg
startButton gameState =
    case gameState of
        Running ->
            Styled.text ""
        Paused ->
            Styled.button [ Events.onClick PauseGame
                          , buttonStyle
                          ]
                          [ Styled.text "Resume" ]
        _ ->
            Styled.button [ Events.onClick StartGame
                          , buttonStyle
                          ]
                          [ Styled.text "New Game" ]


buttonStyle =
    Attr.css [ Css.position Css.absolute
             , Css.left (Css.pct 50)
             , Css.top (Css.pct 50)
             , Css.transform (Css.translate2 (Css.pct -50) (Css.pct -50))
             , Css.zIndex (Css.int 5)
             , Css.backgroundColor (Css.rgb 86 241 208)
             , Css.fontSize (Css.vw 5)
             , Css.borderStyle Css.none
             , Css.color cssWhite
             , Css.padding2 (Css.vh 2) (Css.vw 3)
             , Css.cursor Css.pointer
             , Css.hover buttonHover
             , Css.focus buttonHover
             ]


buttonHover =
    [ Css.backgroundColor (Css.rgb 155 250 230)
    , Css.outline3 (Css.rem 0.2) Css.solid cssBlack
    , Css.color cssBlack
    ]


playerObject : GameObject -> Styled.Html Msg
playerObject { position, velocity } =
    Styled.div [ (gameObjectStyle position.x position.y "%PUBLIC_URL%/player.png")
               ]
               []


enemyObject : GameObject -> Styled.Html Msg
enemyObject { position, velocity } =
    Styled.div [ (gameObjectStyle position.x position.y "%PUBLIC_URL%/falling_object.png")
               ]
               []


gameObjectStyle : Float -> Float -> String -> Styled.Attribute msg
gameObjectStyle x y imageUrl =
    Attr.css [ Css.width (Css.vw unitSize)
             , Css.height (Css.vh unitSize)
             , Css.backgroundImage (Css.url imageUrl)
             , Css.backgroundRepeat Css.noRepeat
             , Css.backgroundSize2 (Css.pct 100) (Css.pct 100)
             , Css.position Css.absolute
             , Css.bottom (Css.vh y)
             , Css.left (Css.vw x)
             ]


---- PROGRAM ----


main : Program Never Model Msg
main =
    Html.program
        { view = view >> Styled.toUnstyled
        , init = init
        , update = update
        , subscriptions = subscriptions
        }


---- SUBSCRIPTIONS ----


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Keyboard.downs (\code -> case (Keys.fromCode code) of
                                        Keys.ArrowLeft ->
                                            Move -1
                                        Keys.ArrowRight ->
                                            Move 1
                                        Keys.CharA ->
                                            Move -1
                                        Keys.CharD ->
                                            Move 1
                                        Keys.Escape ->
                                            PauseGame
                                        _ ->
                                            NoOp
                           )
        , Keyboard.ups (\code -> case (Keys.fromCode code) of
                                        Keys.ArrowLeft ->
                                            Move 0
                                        Keys.ArrowRight ->
                                            Move 0
                                        Keys.CharA ->
                                            Move 0
                                        Keys.CharD ->
                                            Move 0
                                        _ ->
                                            NoOp
                           )
        , Time.every 1800 (\time -> Spawn time)
        , Anim.diffs (\deltaTime -> Tick (deltaTime / 1000))
        , windowBlur LoseFocus
        , receiveScores updateHighScores
        ]


port windowBlur : (String -> msg) -> Sub msg

port saveScore : (String, Int) -> Cmd msg
port getScores : () -> Cmd msg
port receiveScores : (JsonD.Value -> msg) -> Sub msg


updateHighScores : JsonD.Value -> Msg
updateHighScores possibleScores =
    case (JsonD.decodeValue decodeHighScores possibleScores) of
        Ok scores ->
            ReceiveHighScores scores
        Err err ->
            ReceiveHighScores []


decodeHighScores : JsonD.Decoder (List HighScore)
decodeHighScores =
    JsonD.list decodeHighScore


decodeHighScore : JsonD.Decoder HighScore
decodeHighScore =
    JsonD.map2 HighScore
        (JsonD.field "player" JsonD.string)
        (JsonD.field "score" JsonD.int)
