port module App exposing (..)

import Html exposing (..)
import Html.Events exposing (onClick)
import Html.Attributes as HtmlA exposing (style, id)
import Svg exposing (svg, ellipse, line, rect, g, circle)
import Svg.Attributes as SvgA
import Mouse
import Keyboard
import Time
import AnimationFrame as Anim
import Random
import Basics exposing (sin, abs, e)
import Tuple exposing (first,second)
import Array
import Window
import Task

-- MODEL


type alias Model =
    {
        circleYPosition : Float,
        one : (Float,Float),
        two : (Float,Float),
        three : (Float,Float),
        four : (Float,Float),
        five : (Float,Float),
        six : (Float,Float),
        time : Float,
        rand : Float,
        collision : Bool,
        size : Window.Size,
        score : Int
    }


init : ( Model, Cmd Msg )
init =
    ({
        circleYPosition = 50,
        one = (0,50),
        two = (25,20),
        three = (50,40),
        four = (75,75),
        five = (100,30),
        six = (125,30),
        time = 0,
        rand = 0, 
        collision = False,
        size =
        { width = 0
        , height = 0
        },
        score = 0
    },  Task.perform Size Window.size)



-- MESSAGES

type Msg
    = MouseMsg Mouse.Position
    | KeyMsg Keyboard.KeyCode
    | Tick Time.Time
    | Tock Time.Time
    | RequestRandom Float
    | ReceivedRandom Float
    | RequestCollision Float
    | ReceivedCollision Bool
    | Size Window.Size




-- VIEW


--lineCreator : Int -> Int -> (Int, Int)

view : Model -> Html Msg
view model = 
            if model.collision == False
            then
                div [style [("height", "100%"), ("width", "100%"), ("overflow", "hidden"), ("background", "linear-gradient(to top, #f80759, #bc4e9c)") ]] 
                
                [
                    audio [HtmlA.autoplay True, HtmlA.loop True] 
                    [
                        source [HtmlA.src "theme.mp3"] []
                    ], 
                    p [style [("float", "left"), ("margin", "0"), ("font-family", "Arial, Helvetica, sans-serif"), ("font-size", "5vmin"), ("color", "white"), ("position", "fixed")]] [text ("Score: " ++ toString(model.score))],

                    svg [id "svg", style [("height", "100%"), ("width", "100%")], onClick (RequestRandom 0)]
                    [
                        ellipse [SvgA.cx "50%", (SvgA.cy (toString(model.circleYPosition)++"%")), SvgA.rx "100", SvgA.ry "180", SvgA.fill "none", SvgA.stroke "white", SvgA.strokeWidth "10"] [],
                        circle [id "upperHitCircle", SvgA.cx "50%", SvgA.cy (toString(toFloat(model.size.height)*0.01*model.circleYPosition-200)), SvgA.r "30", SvgA.fill "none"] [],
                        circle [id "lowerHitCircle", SvgA.cx "50%", SvgA.cy (toString(toFloat(model.size.height)*0.01*model.circleYPosition+200)), SvgA.r "30", SvgA.fill "none"] [],

                        g [id "track"] 
                        [
                            
                            line [id "line1", SvgA.height "10", SvgA.x1 (toString(first(model.one))++"%"), SvgA.y1 (toString(second(model.one))++"%"), SvgA.x2 (toString(first(model.one)+25)++"%"), SvgA.y2 (toString(second(model.two))++"%"), SvgA.stroke "white", SvgA.strokeWidth "10"] [],
                            line [id "line2", SvgA.height "10", SvgA.x1 (toString(first(model.two))++"%"), SvgA.y1 (toString(second(model.two))++"%"), SvgA.x2 (toString(first(model.two)+25)++"%"), SvgA.y2 (toString(second(model.three))++"%"), SvgA.stroke "white", SvgA.strokeWidth "10"] [],
                            line [id "line3", SvgA.height "10", SvgA.x1 (toString(first(model.three))++"%"), SvgA.y1 (toString(second(model.three))++"%"), SvgA.x2 (toString(first(model.three)+25)++"%"), SvgA.y2 (toString(second(model.four))++"%"), SvgA.stroke "white", SvgA.strokeWidth "10"] [],
                            line [id "line4", SvgA.height "10", SvgA.x1 (toString(first(model.four))++"%"), SvgA.y1 (toString(second(model.four))++"%"), SvgA.x2 (toString(first(model.four)+25)++"%"), SvgA.y2 (toString(second(model.five))++"%"), SvgA.stroke "white", SvgA.strokeWidth "10"] [],
                            line [id "line5", SvgA.height "10", SvgA.x1 (toString(first(model.five))++"%"), SvgA.y1 (toString(second(model.five))++"%"), SvgA.x2 (toString(first(model.five)+25)++"%"), SvgA.y2 (toString(second(model.six))++"%"), SvgA.stroke "white", SvgA.strokeWidth "10"] [],
                            line [id "line6", SvgA.height "10", SvgA.x1 (toString(first(model.six))++"%"), SvgA.y1 (toString(second(model.six))++"%"), SvgA.x2 (toString(first(model.six)+25)++"%"), SvgA.y2 (toString(second(model.one))++"%"), SvgA.stroke "white", SvgA.strokeWidth "10"] []
                          
                        ]
                    ]
                ]
            else
                div [style [("height", "100%"), ("width", "100%"), ("overflow", "hidden"), ("text-align", "center"), ("background", "-webkit-linear-gradient(to top, #f80759, #bc4e9c)"), ("background", "linear-gradient(to top, #f80759, #bc4e9c)"), ("color", "white")]] 
                [
                
                    h1 [] [text "Game Over!"],
                    h2 [] [text ("Your score was " ++ toString(model.score))],
                    br [] [],
                    a [HtmlA.href "./main.html"]
                    [
                        button [] [text "Play again!"]
                    ],

                     audio [id "audio2", HtmlA.autoplay True] 
                    [
                        source [HtmlA.src "lose.mp3"] []
                    ]

                ]



-- UPDATE


port requestRandom : Float -> Cmd msg

port receiveRandom : (Float -> msg) -> Sub msg

port requestCheck : Float -> Cmd msg

port receiveCheck : (Bool -> msg) -> Sub msg

update : Msg -> Model -> ( Model, Cmd Msg )
update msg {circleYPosition, one, two, three, four, five, six, time, rand, collision, size, score} = 
         if first(one) <= -25 then
                ({
                    circleYPosition = circleYPosition,
                    one = (125,rand),
                    two = two,
                    three = three,
                    four = four,
                    five = five,
                    six = six,
                    time = time,
                    rand = rand,
                    collision = collision,
                    size = size,
                    score = score
                }, Cmd.none)
        else if first(two) <= -25 then
                ({
                    circleYPosition = circleYPosition,
                    one = one,
                    two = (125,rand),
                    three = three,
                    four = four,
                    five = five,
                    six = six,
                    time = time,
                    rand = rand,
                    collision = collision,
                    size = size,
                    score = score
                }, Cmd.none)
        else if first(three) <= -25 then
                ({
                    circleYPosition = circleYPosition,
                    one = one,
                    two = two,
                    three = (125,rand),
                    four = four,
                    five = five,
                    six = six,
                    time = time,
                    rand = rand,
                    collision = collision,
                    size = size,
                    score = score
                }, Cmd.none)

        else if first(four) <= -25 then
                ({
                    circleYPosition = circleYPosition,
                    one = one,
                    two = two,
                    three = three,
                    four = (125,rand),
                    five = five,
                    six = six,
                    time = time,
                    rand = rand,
                    collision = collision,
                    size = size,
                    score = score
                }, Cmd.none)
        else if first(five) <= -25 then
                ({
                    circleYPosition = circleYPosition,
                    one = one,
                    two = two,
                    three = three,
                    four = four,
                    five = (125,rand),
                    six = six,
                    time = time,
                    rand = rand,
                    collision = collision,
                    size = size,
                    score = score
                }, Cmd.none)
        else if first(six) <= -25 then
                ({
                    circleYPosition = circleYPosition,
                    one = one,
                    two = two,
                    three = three,
                    four = four,
                    five = five,
                    six = (125,rand),
                    time = time,
                    rand = rand,
                    collision = collision,
                    size = size,
                    score = score
                }, Cmd.none)
        else
            let displacement = 0.3
                jump = 15
            in
                case msg of
                    (MouseMsg _) -> ({
                            circleYPosition = circleYPosition - jump,
                            one = one,
                            two = two,
                            three = three,
                            four = four,
                            five = five,
                            six = six,
                            time = time,
                            rand = rand,
                            collision = collision,
                            size = size,
                            score = score +1
                        }, Cmd.none)
                    (KeyMsg _) -> ({
                            circleYPosition = circleYPosition - jump,
                            one = one,
                            two = two,
                            three = three,
                            four = four,
                            five = five,
                            six = six,
                            time = time,
                            rand = rand,
                            collision = collision,
                            size = size,
                            score = score +1
                        }, Cmd.none)
                    (Tick _) -> ({
                            circleYPosition = circleYPosition + displacement,
                            one = (first(one)-displacement, second(one)),
                            two = (first(two)-displacement, second(two)),
                            three = (first(three)-displacement, second(three)),
                            four = (first(four)-displacement, second(four)),
                            five = (first(five)-displacement, second(five)),
                            six = (first(six)-displacement, second(six)),
                            time = time,
                            rand = rand,
                            collision = collision,
                            size = size,
                            score = score
                        }, requestCheck 0)
                    (Tock _) -> ({
                            circleYPosition = circleYPosition,
                            one = one,
                            two = two,
                            three = three,
                            four = four,
                            five = five,
                            six = six,
                            time = time +1,
                            rand = rand,
                            collision = collision,
                            size = size,
                            score = score
                        }, requestRandom 0)
                    (RequestRandom a) -> ({
                            circleYPosition = circleYPosition,
                            one = one,
                            two = two,
                            three = three,
                            four = four,
                            five = five,
                            six = six,
                            time = time,
                            rand = rand,
                            collision = collision,
                            size = size,
                            score = score
                        }, requestRandom a)
                    (ReceivedRandom a) -> ({
                            circleYPosition = circleYPosition,
                            one = one,
                            two = two,
                            three = three,
                            four = four,
                            five = five,
                            six = six,
                            time = time,
                            rand = a,
                            collision = collision,
                            size = size,
                            score = score
                        }, Cmd.none)
                    (RequestCollision a) -> ({
                            circleYPosition = circleYPosition,
                            one = one,
                            two = two,
                            three = three,
                            four = four,
                            five = five,
                            six = six,
                            time = time,
                            rand = rand,
                            collision = collision,
                            size = size,
                            score = score
                        }, requestCheck a)
                    (ReceivedCollision a) -> ({
                            circleYPosition = circleYPosition,
                            one = one,
                            two = two,
                            three = three,
                            four = four,
                            five = five,
                            six = six,
                            time = time,
                            rand = rand,
                            collision = a,
                            size = size,
                            score = score
                        }, Cmd.none)
                    (Size a) -> ({
                            circleYPosition = circleYPosition,
                            one = one,
                            two = two,
                            three = three,
                            four = four,
                            five = five,
                            six = six,
                            time = time,
                            rand = rand,
                            collision = collision,
                            size = a,
                            score = score
                        }, Cmd.none)


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch 
    [
        Mouse.clicks MouseMsg,
        Keyboard.downs KeyMsg,
        Anim.times Tick,
        Time.every Time.second Tock,
        receiveRandom ReceivedRandom,
        receiveCheck ReceivedCollision
    ]



-- MAIN


main =
    program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }