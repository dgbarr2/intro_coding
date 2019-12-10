var twocircles = '<circle cx="20" cy="20" r="10" stroke="black" stroke-width="2" fill="red"></circle> <circle cx="50" cy="20" r="10" stroke="black" stroke-width="2" fill="green"></circle>'

// Create a dummy receptacle
var receptacle = document.createElement('div');

// Wrap the svg string to a svg object (string)
var svgfragment = '<svg>' + twocircles + '</svg>';

// Add all svg to the receptacle
receptacle.innerHTML = '' + svgfragment;
//alert(SVGElement)

// Splice the childs of the SVG inside the receptacle to the SVG at the body
Array.prototype.slice.call(receptacle.childNodes[0].childNodes).forEach(function (el) {    document.getElementById('svgcanvas').appendChild(el)})
----------------------------------------------------------------------------

function addElement () { 
    // create a new div element 
    var newDiv = document.createElement("div"); 
    // and give it some content 
    var newContent = document.createTextNode("Hi there and greetings!"); 
    // add the text node to the newly created div
    newDiv.appendChild(newContent);  
  
    // add the newly created element and its content into the DOM 
    var currentDiv = document.getElementById("div1"); 
    document.body.insertBefore(newDiv, currentDiv); 
  }

  -------------------------
  var svg = document.getElementsByTagName('svg')[0]; //Get svg element
var newElement = document.createElementNS("http://www.w3.org/2000/svg", 'path'); //Create a path in SVG's namespace
newElement.setAttribute("d","M 0 0 L 10 10"); //Set path's data
newElement.style.stroke = "#000"; //Set stroke colour
newElement.style.strokeWidth = "5px"; //Set stroke width
svg.appendChild(newElement);