    var app = Elm.App.fullscreen();
    function check() {
  var c = document.getElementById("upperHitCircle");
  var c2  = document.getElementById("lowerHitCircle");
  var l = document.getElementById("line1");
  var l2 = document.getElementById("line2");
  var l3 = document.getElementById("line3");
  var l4 = document.getElementById("line4");
  var l5 = document.getElementById("line5");
  var l6 = document.getElementById("line6");

  var circle = { radius : c.r.baseVal.value, center : { x:0, y:0 } };
  var circle2 = { radius : c2.r.baseVal.value, center : { x:0, y:0 } };

  console.log(circle);
  console.log(circle2); 

  circle.center.x = c.cx.baseVal.value;
  circle.center.y = c.cy.baseVal.value;

  circle2.center.x = c2.cx.baseVal.value;
  circle2.center.y = c2.cy.baseVal.value;
  
  var line =   { p1 : { x:l.x1.baseVal.value, y:l.y1.baseVal.value }, p2 : { x:l.x2.baseVal.value, y:l.y2.baseVal.value } };
  var line2 = { p1 : { x:l2.x1.baseVal.value, y:l2.y1.baseVal.value }, p2 : { x:l2.x2.baseVal.value, y:l2.y2.baseVal.value } };
  var line3 = { p1 : { x:l3.x1.baseVal.value, y:l3.y1.baseVal.value }, p2 : { x:l3.x2.baseVal.value, y:l3.y2.baseVal.value } };
  var line4 = { p1 : { x:l4.x1.baseVal.value, y:l4.y1.baseVal.value }, p2 : { x:l4.x2.baseVal.value, y:l4.y2.baseVal.value } };
  var line5 = { p1 : { x:l5.x1.baseVal.value, y:l5.y1.baseVal.value }, p2 : { x:l5.x2.baseVal.value, y:l5.y2.baseVal.value } };
  var line6 = { p1 : { x:l6.x1.baseVal.value, y:l6.y1.baseVal.value }, p2 : { x:l6.x2.baseVal.value, y:l6.y2.baseVal.value } };

  var distance = circleDistFromLineSeg(circle, line);
  var distance2 = circleDistFromLineSeg(circle, line2);
  var distance3 = circleDistFromLineSeg(circle, line3);
  var distance4 = circleDistFromLineSeg(circle, line4);
  var distance5 = circleDistFromLineSeg(circle, line5);
  var distance6 = circleDistFromLineSeg(circle, line6);

  var distance7 = circleDistFromLineSeg(circle2, line);
  var distance8 = circleDistFromLineSeg(circle2, line2);
  var distance9 = circleDistFromLineSeg(circle2, line3);
  var distance10 = circleDistFromLineSeg(circle2, line4);
  var distance11 = circleDistFromLineSeg(circle2, line5);
  var distance12 = circleDistFromLineSeg(circle2, line6);

  distanceArray = [distance, distance2, distance3, distance4, distance5, distance6, distance7, distance8, distance9, distance10, distance11, distance12];
  minDistance = (Math.min.apply(null,distanceArray));

  console.log(minDistance);

  if (minDistance < (circle.radius + 0.5*parseFloat(l.getAttribute('stroke-width')))) {
    console.log("You're dead, mate!");
    return true;
  }
  else {
    return false;
  }


  
  
  
  
}
function circleDistFromLineSeg(circle,line) {
    // SOURCE: https://stackoverflow.com/questions/37224912/circle-line-segment-collision
    var v1, v2, v3, u;
    v1 = {};
    v2 = {};
    v3 = {};
    v1.x = line.p2.x - line.p1.x;
    v1.y = line.p2.y - line.p1.y;
    v2.x = circle.center.x - line.p1.x;
    v2.y = circle.center.y - line.p1.y;
    u = (v2.x * v1.x + v2.y * v1.y) / (v1.y * v1.y + v1.x * v1.x); // unit dist of point on line
    if(u >= 0 && u <= 1){
        v3.x = (v1.x * u + line.p1.x) - circle.center.x;
        v3.y = (v1.y * u + line.p1.y) - circle.center.y;
        v3.x *= v3.x;
        v3.y *= v3.y;
        return Math.sqrt(v3.y + v3.x); // return distance from line
    } 
    // get distance from end points
    v3.x = circle.center.x - line.p2.x;
    v3.y = circle.center.y - line.p2.y;
    v3.x *= v3.x;  // square vectors
    v3.y *= v3.y;    
    v2.x *= v2.x;
    v2.y *= v2.y;
    return Math.min(Math.sqrt(v2.y + v2.x), Math.sqrt(v3.y + v3.x)); // return smaller of two distances as the result
}
 function moveSection(idStr, xOffset, yOffset) {
   var domElemnt = document.getElementById(idStr);
   if (domElemnt) {
     var transformAttr = ' translate(' + xOffset + ',' + yOffset + ')';
     domElemnt.setAttribute('transform', transformAttr);
   }
 }
    app.ports.requestRandom.subscribe(function(x) {
        y = Math.floor(Math.random() * (80 - 20) + 20)
        app.ports.receiveRandom.send(y);
        });
        app.ports.requestCheck.subscribe(function(x) {
        y = check();
        app.ports.receiveCheck.send(y);
    });