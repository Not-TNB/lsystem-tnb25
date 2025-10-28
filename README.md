# L-Systems Parser, Expander \& Renderer
## Haskell Programming Project @ Imperial College London - COMP40009
_"A popular way of defining plant-like structures is to use Lindenmayer Systems, or L-Systems for
short. L-Systems are named after the biologist Aristid Lindenmayer who was interested in describing
the apparently fractal nature of plant growth using simple rewriting rules." - Jamie Willis_

### Brief Intro
This project/exercise in Haskell (whose original skeleton files are written by Jamie Willis) aims to produce renderings of Lindenmayer-Systems (L-Systems) using OpenGL (via GLUT) and turtle-based graphics.
The program, when run appropriately, is able to parse, expand and render well-known L-systems, with the added extensionality feature of line color variance in the rendering (specifically, hue-shifting).

### File Structure \& Usage
The file structure of the project (as given in the original specification) is written below
```
.
├── src
│ ├── IC
│ │ ├── Colour.hs ................. Colour utilities and type definition
│ │ └── Graphics.hs ............... Internal implementation of the rendering
│ ├── Examples.hs ................. Sample well-known L-systems usable from the get-go in terminal
│ ├── LSystems.hs ................. Parsing, Expansion and line-generation code
│ └── Renderer.hs ................. Pre-defined functions for drawing L-Systems (default module)
└── lsystems.cabal
```
To run the project, type the following command into the terminal:
```
cabal repl
```
Access to three commands (seen in `Renderer.hs`) are given. These can be entered after `cabal repl`.

#### `drawLSystem1`
Syntax: `drawLSystem1 liveAction system nIter colour`

Information: This implementation attempts to render via simple recursion.

Attributes:
- `liveAction`: A boolean value, which will show a live animation of the turtle drawing the expanded L-system if set to `True`, and only the final result otherwise
- `system`: The L-system to be drawn; pre-defined L-systems are given in `Examples.hs` but feel free to add in your own!
- `nIter`: An integer determining the number of times to rewrite the L-system from its axiom given the ruleset of the system
- `colour`: The colour to draw the L-system in.

#### `drawLSystem2`
Syntax: `drawLSystem2 liveAction system nIter colour`

Information: This implementation attempts to render via stack operations. 

Atttributes: See `drawLSystem1`.

#### `drawLSystemHS`
Syntax: `drawLSystemHS liveAction system nIter colour shift`
Information: Similar in implementation to `drawLSystemHS` under the hood, but with the ability to hue-shift the colour of the turtle's pen by $\theta$ degrees on each movement.
Attributes:
- `liveAction`, `system`, `nIter`, `colour`: See `drawLSystem1`
- `shift`: The angle $\theta$ in degrees to shift the hue of the turtle's pen colour by on each move. Note that `colour` is hence the initial colour of the turtle's pen

### Colours
The following constructors for `Colour`s are available for use when passing in the `colour` argument for the three L-system draw functions:
- `RGB r g b`: the RGB colour where $0\leq r,g,b \leq 1$ determine the strength of the red, green and blue colour channels respectively.
- `HSV h s v`: the HSV colour where $0\leq s,v\leq 1$; $h$ determines hue (in degrees), $s$ determines saturation and $v$ value.

In addition, the following pre-defined shorthand colours can also be used instead:
- `Black  `: equivalent to `RGB 0 0 0`
- `White  `: equivalent to `RGB 1 1 1`
- `Red    `: equivalent to `RGB 1 0 0`
- `Green  `: equivalent to `RGB 0 1 0`
- `Blue   `: equivalent to `RGB 0 1 1`
- `Cyan   `: equivalent to `RGB 0 1 1`
- `Magenta`: equivalent to `RGB 1 0 1`
- `Yellow `: equivalent to `RGB 1 1 0`

### Defining L-Systems
Within the project, L-systems are a datatype constructed as follows:
```
LSystem angle axiom rules
```
- `angle`: a Float value determining the angle (in degrees) to turn the turtle left or right upon `L` or `R` commands.
- `axiom`: a String which is the initial reading of the L-system before any expansion is done with the ruleset.
- `rules`: the ruleset of the L-system, a list of tuples `(a,b)` where, in each rewriting, any substring `a` is rewritten as `b`; in this sense each tuple in `rules` is one rewrite rule.

For example, the following is how the familiar `tree` L-system is defined with this syntax:
```haskell
tree = LSystem 60 "M" [ ('M', "N[-M][+M][NM]")
                      , ('N', "NN")
                      , ('[', "[")
                      , (']', "]")
                      , ('+', "+")
                      , ('-', "-")
                      ]
```
Note that `[]` represent branched/bracketed commands, `+` represents a rotation to the right, `-` to the left, and both `M` and `N` move the turtle forward by one unit. 
The commands `X`, `Y` and `A` are also available but do not do anything to the turtle.
