module App exposing (..)

import Html exposing (..)
import Html.Events exposing (onClick)
import Html.Attributes exposing (style, id)
import Svg exposing (svg, ellipse, line)
import Svg.Attributes as SvgA
import Mouse
import Time
import AnimationFrame as Anim
import Random
import Basics exposing (sin, abs)
import Tuple exposing (first,second)

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
        time : Float
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
        time = 0
    },  Cmd.none )



-- MESSAGES

type Msg
    = MouseMsg Mouse.Position
    | Tick Time.Time
    | Tock Time.Time




-- VIEW


--lineCreator : Int -> Int -> (Int, Int)

view : Model -> Html Msg
view model = 


            div [style [("height", "100%"), ("width", "100%")]] 
            
            [
                svg [style [("height", "100%"), ("width", "100%")]]
                [
                    ellipse [SvgA.cx "50vw", (SvgA.cy (toString(model.circleYPosition)++"vh")), SvgA.rx "100", SvgA.ry "180", SvgA.fill "none", SvgA.stroke "black", SvgA.strokeWidth "10"] [],
                    line [SvgA.x1 (toString(first(model.one))++"vw"), SvgA.y1 (toString(second(model.one))++"vh"), SvgA.x2 (toString(first(model.one) + 25)++"vw"), SvgA.y2 (toString(second(model.two))++"vh"), SvgA.stroke "black", SvgA.strokeWidth "10"] [],
                    line [SvgA.x1 (toString(first(model.two))++"vw"), SvgA.y1 (toString(second(model.two))++"vh"), SvgA.x2 (toString(first(model.two) + 25)++"vw"), SvgA.y2 (toString(second(model.three))++"vh"), SvgA.stroke "black", SvgA.strokeWidth "10"] [],
                    line [SvgA.x1 (toString(first(model.three))++"vw"), SvgA.y1 (toString(second(model.three))++"vh"), SvgA.x2 (toString(first(model.three) + 25)++"vw"), SvgA.y2 (toString(second(model.four))++"vh"), SvgA.stroke "black", SvgA.strokeWidth "10"] [],
                    line [SvgA.x1 (toString(first(model.four))++"vw"), SvgA.y1 (toString(second(model.four))++"vh"), SvgA.x2 (toString(first(model.four) + 25)++"vw"), SvgA.y2 (toString(second(model.five))++"vh"), SvgA.stroke "black", SvgA.strokeWidth "10"] [],
                    line [SvgA.x1 (toString(first(model.five))++"vw"), SvgA.y1 (toString(second(model.five))++"vh"), SvgA.x2 (toString(first(model.five) + 25)++"vw"), SvgA.y2 (toString(second(model.six))++"vh"), SvgA.stroke "black", SvgA.strokeWidth "10"] [],
                    line [SvgA.x1 (toString(first(model.six))++"vw"), SvgA.y1 (toString(second(model.six))++"vh"), SvgA.x2 (toString(first(model.six) + 25)++"vw"), SvgA.y2 (toString(second(model.one))++"vh"), SvgA.stroke "black", SvgA.strokeWidth "10"] []
                ]
            ]



-- UPDATE



update : Msg -> Model -> ( Model, Cmd Msg )
update msg {circleYPosition, one, two, three, four, five, six, seven, time} = 
         if first(one) < -25 then
                ({
                    circleYPosition = circleYPosition,
                    one = (125,abs(sin(time))*100),
                    two = two,
                    three = three,
                    four = four,
                    five = five,
                    six = six,
                    seven = seven,
                    time = time
                }, Cmd.none)
        else if first(two) < -25 then
                ({
                    circleYPosition = circleYPosition,
                    one = one,
                    two = (125,abs(sin(time+1))*100),
                    three = three,
                    four = four,
                    five = five,
                    six = six,
                    seven = seven,
                    time = time
                }, Cmd.none)
        else if first(three) < -25 then
                ({
                    circleYPosition = circleYPosition,
                    one = one,
                    two = two,
                    three = (125,abs(sin(time+2))*100),
                    four = four,
                    five = five,
                    six = six,
                    seven = seven,
                    time = time
                }, Cmd.none)

        else if first(four) < -25 then
                ({
                    circleYPosition = circleYPosition,
                    one = one,
                    two = two,
                    three = three,
                    four = (125,abs(sin(time+3))*100),
                    five = five,
                    six = six,
                    seven = seven,
                    time = time
                }, Cmd.none)
        else if first(five) < -25 then
                ({
                    circleYPosition = circleYPosition,
                    one = one,
                    two = two,
                    three = three,
                    four = four,
                    five = (125,abs(sin(time+4))*100),
                    six = six,
                    seven = seven,
                    time = time
                }, Cmd.none)
        else if first(six) < -25 then
                ({
                    circleYPosition = circleYPosition,
                    one = one,
                    two = two,
                    three = three,
                    four = four,
                    five = five,
                    six = (125,abs(sin(time+5))*100),
                    seven = seven,
                    time = time
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
                    seven = (125, abs(sin(time+6))*100),
                    time = time
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
                        time = time
                    }, Cmd.none)
                (Tick _) -> ({
                        circleYPosition = circleYPosition + 0.05,
                        one = (first(one)-0.05, second(one)),
                        two = (first(two)-0.05, second(two)),
                        three = (first(three)-0.05, second(three)),
                        four = (first(four)-0.05, second(four)),
                        five = (first(five)-0.05, second(five)),
                        six = (first(six)-0.05, second(six)),
                        seven = (first(seven)-0.05, second(seven)),
                        time = time
                    }, Cmd.none)
                (Tock _) -> ({
                        circleYPosition = circleYPosition,
                        one = one,
                        two = two,
                        three = three,
                        four = four,
                        five = five,
                        six = six,
                        seven = seven,
                        time = time +1
                    }, Cmd.none)


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch 
    [
        Mouse.clicks MouseMsg,
        Anim.times Tick,
        Anim.times Tock
    ]



-- MAIN


main =
    program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }