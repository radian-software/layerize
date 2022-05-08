Check out the video [here](https://youtu.be/SpUkQ700BqQ).

# General info

In the fall of my senior year in high school, I took a class called
Design Technology 2. For my final project, I constructed a
three-dimensional model of a certain irregular solid out of
cross-sections, using the laser cutter. It would have been impossible
to design the schematics by hand, so I wrote this library to generate
them for me. You can see a making-of video at my
website [here][other projects].

The functions of each of the files in `src/layerize` are as follows:
- `applet` is a Quil applet that visualizes cross sections of the
  solid and shows the tracing process; it is interactive and requires
  keyboard input
- `bezier` contains an algorithm for drawing smooth (Bézier) curves
  through points; it is unused
- `core` is the main namespace and contains routines for generating
  the schematics
- `equation` contains the equations that define the solid
- `problems` contains some routines for identifying places where the
  generated pieces would intersect (and might need to be filed down or
  recut); it was written in a panic when I realized that such
  intersections could happen
- `schematic` contains the logic for placing grooves in the
  cross-sectional pieces
- `svg` converts schematics to SVG code for the laser cutter
- `trace` converts a collection of points representing a cross section
  into an ordered list of points that can be used as a path
- `util` contains general utility functions, including the algorithm
  used to group cross sections into disjoint pieces as well as the
  routines used to deal with viewing frames in the applet

This is relatively old code. Also, it was written pretty quickly, in a
shorter amount of time than [MazeGen][mazegen]. As a consequence, it
doesn't have nice, easy-to-use endpoints. Also, there are a number of
problems that I handled by adjusting the output manually. Unlike
MazeGen, it is probably not suitable for use by anyone other than me.
However, if you are interested in this project, please feel free to
contact me at [contact+layerize@radian.codes][email] and I would be
happy to answer any questions you might have.

## Miscellany

- To generate the schematics, I think you have to call `core/pieces`
  and pass the result to `svg/pieces->svg`.
- The pieces are not arranged neatly on the canvas as in MazeGen. You
  will have to arrange them manually.
- This project requires significant improvements to the grooving
  algorithm in `schematic`. Without them, it generates a number of the
  pieces so that they intersect with other pieces. I had to manually
  adjust and recut quite a few pieces.
- The solid can be viewed in Grapher using the file `MobiusSolid.gcx`.

[email]: mailto:contact+layerize@radian.codes
[mazegen]: https://github.com/radian-software/MazeGen
[other projects]: https://intuitiveexplanations.com/other-projects/
