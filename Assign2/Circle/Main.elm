port module App exposing (..)

import Html exposing (..)
import Html.Events exposing (onClick)
import Html.Attributes as HtmlA exposing (style, id)
import Svg exposing (svg, ellipse, line, rect, g, circle)
import Svg.Attributes as SvgA
import Mouse
import Time
import AnimationFrame as Anim
import Random
import Basics exposing (sin, abs)
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
        seven : (Float, Float),
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
        six = (125,50),
        seven = (150,20),
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
    | Tick Time.Time
    | Tock Time.Time
    | Request Float
    | Received Float
    | RequestCollision Float
    | ReceivedCollision Bool
    | Size Window.Size
    | UpdateScore




-- VIEW


--lineCreator : Int -> Int -> (Int, Int)

view : Model -> Html Msg
view model = 
            if model.collision == False
            then
                div [style [("height", "100%"), ("width", "100%"), ("overflow", "hidden")]] 
                
                [
                    p [style [("float", "left"), ("margin", "0")]] [text (toString(model.score))],
                    p [style [("float", "right"), ("margin", "0")]] [text (toString((model.collision)))],

                    svg [id "svg", style [("height", "100%"), ("width", "100%")], onClick (Request 0)]
                    [
                        ellipse [SvgA.cx "50%", (SvgA.cy (toString(model.circleYPosition)++"%")), SvgA.rx "100", SvgA.ry "180", SvgA.fill "none", SvgA.stroke "black", SvgA.strokeWidth "10"] [],
                        circle [id "upperHitCircle", SvgA.cx "50%", SvgA.cy (toString(toFloat(model.size.height)*0.01*model.circleYPosition-200)), SvgA.r "30", SvgA.fill "none"] [],
                        circle [id "lowerHitCircle", SvgA.cx "50%", SvgA.cy (toString(toFloat(model.size.height)*0.01*model.circleYPosition+200)), SvgA.r "30", SvgA.fill "none"] [],

                        g [id "track"] 
                        [
                            
                            line [id "line1", SvgA.height "10", SvgA.x1 (toString(first(model.one))++"%"), SvgA.y1 (toString(second(model.one))++"%"), SvgA.x2 (toString(first(model.one) + 25)++"%"), SvgA.y2 (toString(second(model.two))++"%"), SvgA.stroke "black", SvgA.strokeWidth "10"] [],
                            line [id "line2", SvgA.height "10", SvgA.x1 (toString(first(model.two))++"%"), SvgA.y1 (toString(second(model.two))++"%"), SvgA.x2 (toString(first(model.two) + 25)++"%"), SvgA.y2 (toString(second(model.three))++"%"), SvgA.stroke "black", SvgA.strokeWidth "10"] [],
                            line [id "line3", SvgA.height "10", SvgA.x1 (toString(first(model.three))++"%"), SvgA.y1 (toString(second(model.three))++"%"), SvgA.x2 (toString(first(model.three) + 25)++"%"), SvgA.y2 (toString(second(model.four))++"%"), SvgA.stroke "black", SvgA.strokeWidth "10"] [],
                            line [id "line4", SvgA.height "10", SvgA.x1 (toString(first(model.four))++"%"), SvgA.y1 (toString(second(model.four))++"%"), SvgA.x2 (toString(first(model.four) + 25)++"%"), SvgA.y2 (toString(second(model.five))++"%"), SvgA.stroke "black", SvgA.strokeWidth "10"] [],
                            line [id "line5", SvgA.height "10", SvgA.x1 (toString(first(model.five))++"%"), SvgA.y1 (toString(second(model.five))++"%"), SvgA.x2 (toString(first(model.five) + 25)++"%"), SvgA.y2 (toString(second(model.six))++"%"), SvgA.stroke "black", SvgA.strokeWidth "10"] [],
                            line [id "line6", SvgA.height "10", SvgA.x1 (toString(first(model.six))++"%"), SvgA.y1 (toString(second(model.six))++"%"), SvgA.x2 (toString(first(model.six) + 25)++"%"), SvgA.y2 (toString(second(model.one))++"%"), SvgA.stroke "black", SvgA.strokeWidth "10"] []
                          
                        ]
                    ]
                ]
            else
                div [style [("height", "100%"), ("width", "100%"), ("overflow", "hidden")]] 
                [
                    h1 [] [text "Game Over!"],
                    h2 [] [text ("Your score was " ++ toString(model.score))],
                    br [] [],
                    a [HtmlA.href "./index.html"]
                    [
                        button [] [text "Play again!"]
                    ]

                ]



-- UPDATE


port requestRandom : Float -> Cmd msg

port receiveRandom : (Float -> msg) -> Sub msg

port requestCheck : Float -> Cmd msg

port receiveCheck : (Bool -> msg) -> Sub msg

request x = (Request 0)

update : Msg -> Model -> ( Model, Cmd Msg )
update msg {circleYPosition, one, two, three, four, five, six, seven, time, rand, collision, size, score} = 
         if first(one) < -25 then
                ({
                    circleYPosition = circleYPosition,
                    one = (125,rand),
                    two = two,
                    three = three,
                    four = four,
                    five = five,
                    six = six,
                    seven = seven,
                    time = time,
                    rand = rand,
                    collision = collision,
                    size = size,
                    score = score
                }, Cmd.none)
        else if first(two) < -25 then
                ({
                    circleYPosition = circleYPosition,
                    one = one,
                    two = (125,rand),
                    three = three,
                    four = four,
                    five = five,
                    six = six,
                    seven = seven,
                    time = time,
                    rand = rand,
                    collision = collision,
                    size = size,
                    score = score
                }, Cmd.none)
        else if first(three) < -25 then
                ({
                    circleYPosition = circleYPosition,
                    one = one,
                    two = two,
                    three = (125,rand),
                    four = four,
                    five = five,
                    six = six,
                    seven = seven,
                    time = time,
                    rand = rand,
                    collision = collision,
                    size = size,
                    score = score
                }, Cmd.none)

        else if first(four) < -25 then
                ({
                    circleYPosition = circleYPosition,
                    one = one,
                    two = two,
                    three = three,
                    four = (125,rand),
                    five = five,
                    six = six,
                    seven = seven,
                    time = time,
                    rand = rand,
                    collision = collision,
                    size = size,
                    score = score
                }, Cmd.none)
        else if first(five) < -25 then
                ({
                    circleYPosition = circleYPosition,
                    one = one,
                    two = two,
                    three = three,
                    four = four,
                    five = (125,rand),
                    six = six,
                    seven = seven,
                    time = time,
                    rand = rand,
                    collision = collision,
                    size = size,
                    score = score
                }, Cmd.none)
        else if first(six) < -25 then
                ({
                    circleYPosition = circleYPosition,
                    one = one,
                    two = two,
                    three = three,
                    four = four,
                    five = five,
                    six = (125,rand),
                    seven = seven,
                    time = time,
                    rand = rand,
                    collision = collision,
                    size = size,
                    score = score
                }, Cmd.none)
        else if first(seven) < -25 then
                ({
                    circleYPosition = circleYPosition,
                    one = one,
                    two = two,
                    three = three,
                    four = four,
                    five = five,
                    six = six,
                    seven = (125, rand),
                    time = time,
                    rand = rand,
                    collision = collision,
                    size = size,
                    score = score
                }, Cmd.none)
        else
            case msg of
                (MouseMsg _) -> ({
                        circleYPosition = circleYPosition - 10,
                        one = one,
                        two = two,
                        three = three,
                        four = four,
                        five = five,
                        six = six,
                        seven = seven,
                        time = time,
                        rand = rand,
                        collision = collision,
                        size = size,
                        score = score +1
                    }, Cmd.none)
                (Tick _) -> ({
                        circleYPosition = circleYPosition + 0.3,
                        one = (first(one)-0.3, second(one)),
                        two = (first(two)-0.3, second(two)),
                        three = (first(three)-0.3, second(three)),
                        four = (first(four)-0.3, second(four)),
                        five = (first(five)-0.3, second(five)),
                        six = (first(six)-0.3, second(six)),
                        seven = (first(seven)-0.3, second(seven)),
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
                        seven = seven,
                        time = time +1,
                        rand = rand,
                        collision = collision,
                        size = size,
                        score = score
                    }, requestRandom 0)
                (Request a) -> ({
                        circleYPosition = circleYPosition,
                        one = one,
                        two = two,
                        three = three,
                        four = four,
                        five = five,
                        six = six,
                        seven = seven,
                        time = time,
                        rand = rand,
                        collision = collision,
                        size = size,
                        score = score
                    }, requestRandom a)
                (Received a) -> ({
                        circleYPosition = circleYPosition,
                        one = one,
                        two = two,
                        three = three,
                        four = four,
                        five = five,
                        six = six,
                        seven = seven,
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
                        seven = seven,
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
                        seven = seven,
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
                        seven = seven,
                        time = time,
                        rand = rand,
                        collision = collision,
                        size = a,
                        score = score
                    }, Cmd.none)
                (UpdateScore) -> ({
                        circleYPosition = circleYPosition,
                        one = one,
                        two = two,
                        three = three,
                        four = four,
                        five = five,
                        six = six,
                        seven = seven,
                        time = time,
                        rand = rand,
                        collision = collision,
                        size = size,
                        score = score + 1
                    }, Cmd.none)


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch 
    [
        Mouse.clicks MouseMsg,
        Anim.times Tick,
        Time.every Time.second Tock,
        receiveRandom Received,
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