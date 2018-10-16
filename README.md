### Description

My Haskell implementation for this [Geometric Intro](https://github.com/penrose/GraphicsAPIDocs/blob/master/GeometryIntro.pdf) problem set by Katherine. Haskell code is in `PointsAndLines.hs` and `Polygons.hs`.

It also contains a JavaScript visualizer of a subset of the functions.

### Usage for the visualizer

##### Installation

It doesn't require any installation if you only want to see the visualization of the current Haskell code. Just download the repo and double click on `index.html` to run.

However if any Haskell code is changed, the new code needs to be re-compiled using Fay in order to show effect in the visualizer. Installation instruction for Fay can be found [here](https://github.com/faylang/fay/wiki). 

To re-compile with Fay, go to the directory of this repo and run

`fay main.hs --strict PointsAndLines,Polygons`

And then double click on `index.html`.

p.s. Please don't change any function names, unless in `p5sketch.js` in the testing functions list (starting on line 29) the name is also changed accordingly.


##### Usage

To make elements: press `1`, `2` or `3` to start recording.

`1`: record a point. Click on where to place it.  
`2`: record a segment. Click twice to specify its endpoints.  
`3`: record a polygon. Click to specify its vertices in order. When done, press `c` (close).

When not recording, click on an element to toggle selection. If clicked at places where elements overlap, it toggles all elements underneath. Selected elements are listed in order on the left of the canvas.

You can press `d` to delete selected elements.

Click on a function on top left (whose required arguments are also listed).

Press `space` to feed currently selected list of elements as input to the selected function. Order of arguments matter (otherwise it crashes).

![A screenshot](https://miyehn.me/files/screenshot.png)

Note: some results show up as new elements added onto the canvas, but some only show as text output in the console.


### More screenshots from visualizer

![screenshot 2](https://miyehn.me/files/screenshot2.png)

![screenshot 3](https://miyehn.me/files/screenshot3.png)

![screenshot 4](https://miyehn.me/files/screenshot4.png)

![screenshot 5](https://miyehn.me/files/screenshot5.png)



### Tools used

[Fay](https://github.com/faylang/fay/wiki)

[p5.js](https://p5js.org/)