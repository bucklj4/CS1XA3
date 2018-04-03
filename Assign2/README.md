# Circle #

Circle is a simple game: You move the circle by pressing any key or clicking the keyboard. The objective of the game is simple: if the circle's top or bottom hits the line it its middle, it's game over!

## The Code ##

Circle is a HTML5 web game created using the Elm language and JavaScript. All of the graphical elements of the game are drawn as SVGs using Elm. To create the illusion of a never-ending game, six SVG lines of a width of 25% of the viewport's width, are displayed in a loop. That means there are four lines on screen at once while there is a line to the left of the screen and another to the right. Whenever a line is completely off the left-hand side of the screen, that means it's left x-coordinate is -25%, it is teleported to the other side of the screen with an x-coordinate of 125%.

To make the lines move to the left screen, an Animation Frame is used in Elm to acheive smooth moving. Apart from the initial y-coordinates of the six lines, each new y-coordinate of a line is determined using a random number generator in JavaScript. A port from Elm allows the retrieval of this random number (JavaScript was used in lieu of Elm because, as a functional language, Elm's random numbers are not very random because they need a seed to be given).

Lastly, to check the collision of the circle with the line its middle, JavaScript is used. Using code from a StackOverflow user, we can determine the distance of a circle to a line. This in turn allowed me to use two hidden circles placed on the top and on the bottom of the main circle to check for collision with the line (using the circle's radius and the width of the line).

To check out the game yourself, head over to: http://ugweb.cas.mcmaster.ca/~bucklj4/ and click Circle under my Projects.

For more specifics of how the code works, read the comments in Main.elm and script.js

## Credits ##

#### Circle Collision Code ####

The circle collision code in script.js is a modified version of Joseph Stevens's code found on StackOverflow: https://stackoverflow.com/questions/49432009/how-to-determine-when-rectangle-and-rotated-rectangle-intersect-using-javascript/49434127#49434127

#### Music ####

The game music is from the brilliant game show "Who Wants to Be a Millionaire":
https://www.youtube.com/watch?v=Fcor_OScE0k&t=203s
https://www.youtube.com/watch?v=S6zwxUjPYWw

#### Copyright ####

Game Idea: Ketchapp's Circle https://itunes.apple.com/ca/app/circle/id911152486?mt=8
Collision Code: Joseph Stevens / Jack Buckley
Original Code: Jack Buckley


