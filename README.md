### Description

My Haskell implementation for some geometric queries, starting from this [Geometric Intro](https://github.com/penrose/GraphicsAPIDocs/blob/master/GeometryIntro.pdf) problem set by Katherine. Haskell code is in `PointsAndLines.hs` and `Polygons.hs`.

It also contains a JavaScript visualizer for some of the functions.

#### current progress

Changed from using Fay to using a small server (the Fay version is in Fay branch). Implementation now runs directly in Haskell. Also to test a function, press `enter` instead of `space` because `space` also scrolls the page. 

TODO: add distance functions and clean up old code.

### Usage for the visualizer

#### Get it running

(note: haven't tried running the binary from a computer without GHC installed.)

From command line, run `./main` to start the server. Then in a browser go to localhost:9200

If any change is made to the Haskell code, need to recompile to update the change in visualizer.

Alternatively, load `main.hs` into ghci and run the `main` function.

p.s. Please don't change any function names in Haskell code.

#### Usage

To make elements: press `1`, `2` or `3` to start recording.

`1`: record a point. Click on where to place it.  
`2`: record a segment. Click twice to specify its endpoints.  
`3`: record a polygon. Click to specify its vertices in order. When done, press `c` (close).

When not recording, click on an element to toggle selection. If clicked at places where elements overlap, it toggles all elements underneath. Selected elements are listed in order on the left of the canvas.

You can press `d` to delete selected elements.

Click on a function on top left (whose required arguments are also listed).

Press `Enter` to feed currently selected list of elements as input to the selected function. Order of arguments matter (otherwise it crashes).

![A screenshot](https://miyehn.me/files/screenshot.png)

Note: some results show up as new elements added onto the canvas, but some only show as text output in the console.


### More screenshots from visualizer

![screenshot 2](https://miyehn.me/files/screenshot2.png)

![screenshot 3](https://miyehn.me/files/screenshot3.png)

![screenshot 4](https://miyehn.me/files/screenshot4.png)

![screenshot 5](https://miyehn.me/files/screenshot5.png)



### Tools used

[Scotty](https://github.com/scotty-web/scotty)

[p5.js](https://p5js.org/)