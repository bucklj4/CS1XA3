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
import Tuple exposing (first,second)
import Window
import Task

-- MODEL

-- In this code, we means me, the programmer, and you, the code viewer. We're a team :)

-- Model contains the y-position of the circle, six of the lines' x- and y-coordinates, a random number, a collision Boolean, the size of the web browser, the score, and a timer
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


-- The initial x- and y-coordinates of the lines are given in percentages. That is, line three has an coordinate of 50% of the screen's widthh 40% of its height
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

-- We need messages for when the keyboard or mouse is activated, whenever the timers' activate, when we need to receive or request a random number or if the collision Boolean is true, and the size of the window
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
            -- Displays when the game is running (the circle hasn't collided with the line yet)
            if model.collision == False
            then
                div [style [("height", "100%"), ("width", "100%"), ("overflow", "hidden"), ("background", "linear-gradient(to top, #f80759, #bc4e9c)") ]] 
                
                [
                    -- Background music
                    audio [HtmlA.autoplay True, HtmlA.loop True] 
                    [
                        source [HtmlA.src "theme.mp3"] []
                    ], 

                    -- Displays the score in the top left corner
                    p [style [("float", "left"), ("margin", "0"), ("font-family", "Arial, Helvetica, sans-serif"), ("font-size", "5vmin"), ("color", "white"), ("position", "fixed")]] [text ("Score: " ++ toString(model.score))],

                    
                    svg [id "svg", style [("height", "100%"), ("width", "100%")]]
                    [
                        -- The main circle
                        ellipse [SvgA.cx "50%", (SvgA.cy (toString(model.circleYPosition)++"%")), SvgA.rx "100", SvgA.ry "180", SvgA.fill "none", SvgA.stroke "white", SvgA.strokeWidth "10"] [],
                        
                        -- These two circles are used as hit boxes and are placed on the top and bottom of the main circle (change the fill to a colour to see). We need to convert screen percentages to pixels here, so that is why we need the size of the display window
                        circle [id "upperHitCircle", SvgA.cx "50%", SvgA.cy (toString(toFloat(model.size.height)*0.01*model.circleYPosition-200)), SvgA.r "30", SvgA.fill "none"] [],
                        circle [id "lowerHitCircle", SvgA.cx "50%", SvgA.cy (toString(toFloat(model.size.height)*0.01*model.circleYPosition+200)), SvgA.r "30", SvgA.fill "none"] [],

                        g [id "track"] 
                        [
                            -- These are the six lines (They run in a loop. Each line has a width of 25% of the screen's width, while the y-coordinate is determined randomly)
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
                -- Displays if the game is over
                div [style [("height", "100%"), ("width", "100%"), ("overflow", "hidden"), ("text-align", "center"), ("background", "-webkit-linear-gradient(to top, #f80759, #bc4e9c)"), ("background", "linear-gradient(to top, #f80759, #bc4e9c)"), ("color", "white")]] 
                [
                
                    h1 [] [text "Game Over!"],
                    h2 [] [text ("Your score was " ++ toString(model.score))],
                    br [] [],
                    a [HtmlA.href "./main.html"]
                    [
                        button [] [text "Play again!"]
                    ],

                    -- The game lost music
                     audio [id "audio2", HtmlA.autoplay True] 
                    [
                        source [HtmlA.src "lose.mp3"] []
                    ]

                ]



-- UPDATE

-- Asks for a random number from the JavaScript DOM (Elm Random was tried here, but is not a true random that we need for a game like this because a seed is needed)
port requestRandom : Float -> Cmd msg

-- Returns the random number to Elm as a float
port receiveRandom : (Float -> msg) -> Sub msg

-- Asks the JavaScript DOM if the circle has collided with any of the lines. This is done by using the two hit circles and a handy formula. Check out script.js
port requestCheck : Float -> Cmd msg

-- Returns the collision state to ELm as a boolean
port receiveCheck : (Bool -> msg) -> Sub msg

update : Msg -> Model -> ( Model, Cmd Msg )
update msg {circleYPosition, one, two, three, four, five, six, time, rand, collision, size, score} = 
         
         -- Here's how the lines loop: whenever a line is completely off screen (the left x-coordinate is -25% of the screen's width), we teleport the line back to the other side of the screen (so it's left x-cooridnate is 25%)
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
            
            -- Fixed variables that we can change to make the game easier or harder.
            let displacement = 0.3
                jump = 15
            in
                case msg of
                    -- Whenever the mouse or a key is clicked, we change the circle's y-position (remember in SVG that the positive y-axis points downwards from the top of the screen: that is why we are subtracting here) 
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

                    -- This is how we are making the line's move left. It's tied to an Animation Frame so it's nice and smooth (and runs very quickly!)
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

                    -- We're requesting random numbers in a seperate timer than the Animation Frame because we only need a new one every second or so (and using an Animation Frame would make the game unplayable due to taxing resources)
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

                    -- Gets the size of the window (only run on startup. We could subscribe to changes, but we don't want to tax resources because we assume the player won't change the window during game play)
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