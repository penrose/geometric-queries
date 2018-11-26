## Description

My Haskell implementation for some geometric queries, starting from this [Geometric Intro](https://github.com/penrose/GraphicsAPIDocs/blob/master/GeometryIntro.pdf) problem set by Katherine. Haskell code is in `PointsAndLines.hs`, `Polygons.hs` and `Gradients.hs`.

It also contains a JavaScript visualizer for some of the functions.

#### current progress

Mostly working on gradients for now. Here's what I have so far, although imperfect and sometimes oscillates.
* input a point `p` and a segment `xy`:
  * move `p` to decrease distance between the two
  * move `xy` to decrease distance between the two
  * rotate `xy` around its midpoint to decrease distance
* input a point `p`, a segment `xy`, and a point `c`:
  * rotate `xy` around `c` to decrease distance between `xy` and `p`
* input a point `p` and a pair of segments (call it `s` for here)
  * move `p` to decrease distance between `p` and `s`
  * move `s` to decrease distance between `p` and `s`

## Usage for the visualizer

### Get it running

(note: haven't tried running the binary from a computer without GHC installed.)

From command line, run `./main` to start the server. Then in a browser go to localhost:9200

If any change is made to the Haskell code, need to recompile with `ghc --make main.hs` to update the change in visualizer.

Alternatively, load `main.hs` into ghci and run the `main` function.

p.s. Please don't change any function names in Haskell code.

### Usage

To make elements: press `1`, `2` or `3` to start recording.

`1`: record a point. Click on where to place it.  
`2`: record a segment. Click twice to specify its endpoints.  
`3`: record a polygon. Click to specify its vertices in order. When done, press `c` (close).

Or can also manually enter elements from the input field at the bottom. Format: points separated by commas, each point in form `[x,y]`. 

1 point: point;  
2 points: segment;  
3 or more points: polygon

When not recording, click on an element to toggle selection. If clicked at places where elements overlap, it toggles all elements underneath. Selected elements are listed in order on the left of the canvas.

Press `d` to delete selected elements.

#### (global var) mode = 0: visualize queries

Click on a function on top left (whose required arguments are also listed).

Press `0` to feed currently selected list of elements as input to the selected function. Order of arguments matter (otherwise it crashes).

![A screenshot](https://miyehn.me/files/screenshot.png)

Note: some results show up as new elements added onto the canvas, but some only show as text output in the console.

#### (global var) mode = 1: gradient descent (default)

Click on a function on top left (whose required arguments are also listed).

Press `v` to make 1 step of descent.

Press `b` to descend until either (1) it converges, or (2) reaches max number of steps (500).

If input is a point and a segment, or a point of two segments, can also press `g` to show a rough graph of distance on rotation of segment(s).

![A screenshot](https://miyehn.me/files/screenshot-rotxyc.gif)

### More screenshots from visualizer

![screenshot 2](https://miyehn.me/files/screenshot2.png)

![screenshot 3](https://miyehn.me/files/screenshot3.png)

![screenshot 4](https://miyehn.me/files/screenshot4.png)

![screenshot 5](https://miyehn.me/files/screenshot5.png)



### Tools used

[Scotty](https://github.com/scotty-web/scotty)

[p5.js](https://p5js.org/)