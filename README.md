# L-Systems Parser, Expander \& Renderer
## Haskell Programming Project @ Imperial College London - COMP40009
_"A popular way of defining plant-like structures is to use Lindenmayer Systems, or L-Systems for
short. L-Systems are named after the biologist Aristid Lindenmayer who was interested in describing
the apparently fractal nature of plant growth using simple rewriting rules." - Jamie Willis_
<hr>

### Brief Intro
This project/exercise in Haskell (whose original skeleton files are written by Jamie Willis) aims to produce renderings of Lindenmayer-Systems (L-Systems) using OpenGL (via GLUT) and turtle-based graphics.
The program, when run appropriately, is able to parse, expand and render well-known L-systems, with the added extensionality feature of line color variance in the rendering (specifically, hue-shifting). 


This project also introduced to me and other students in the Imperial JMC course the basic concepts of benchmarking with `Tasty.Bench` and optimizing code (see further down for benchmarking results!)

<img alt="example" src="https://github.com/user-attachments/assets/8dc2145d-469a-4283-8644-528eef7b5bff" style="width: 70%"><br>
> Example output rendering for the `tree` L-system after 12 rewritings

<hr>

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

<hr>

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

<hr>

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

<hr>

### Examples
Try any of the following example commands! The picture at the beginning of this README is generated by the last command below.
```haskell
drawLSystemHS False canopy    6  (RGB (30/255) (176/255) (25/255))   1
drawLSystemHS True  arrowHead 7  (RGB (240/255) (152/255) (115/255)) 0.07
drawLSystemHS False dragon    16 Green                               (-0.2)
drawLSystemHS False tree      12 Green                               0.15
```

<hr>

### Benchmarking Results
Benchmarks were written and run using `cabal bench`:
```haskell
benchmarks :: Benchmark
benchmarks = bgroup "lsystems"
  [
    bgroup "canopy" [                                   -- WEDNESDAY        | THURSDAY
      bgroup "canopy_8" [                               --                  |
        bench "trace1_canopy_8" (nf benchCan1 canCmd8), --                  | 3.117 s ± 129 ms
        bench "trace2_canopy_8" (nf benchCan2 canCmd8)  --                  | 2.822 s ± 257 ms
      ],        
      bgroup "canopy_6" [                               --                  |
        bench "trace1_canopy_6" (nf benchCan1 canCmd6), -- 79.7 ms ± 4.8 ms | 31.8 ms ± 1.7 ms
        bench "trace2_canopy_6" (nf benchCan2 canCmd6)  -- 182  ms ±  18 ms | 28.4 ms ± 1.8 ms
      ],                                                --                  |
      bgroup "canopy_3" [                               --                  |
        bench "trace1_canopy_3" (nf benchCan1 canCmd3), -- 34.1 μs ± 1.9 μs | 38.3 μs ± 2.0 μs
        bench "trace2_canopy_3" (nf benchCan2 canCmd3)  -- 38.2 μs ± 3.7 μs | 32.2 μs ± 1.8 μs
      ],                                                --                  |
      bgroup "canopy_1" [                               --                  |
        bench "trace1_canopy_1" (nf benchCan1 canCmd1), -- 367  ns ±  26 ns | 379  ns ±  26 ns
        bench "trace2_canopy_1" (nf benchCan2 canCmd1)  -- 357  ns ±  34 ns | 341  ns ±  26 ns
      ]
    ],

    bgroup "dragon" [                                   -- WEDNESDAY        | THURSDAY
      bgroup "dragon_6" [                               --                  |
        bench "trace1_dragon_6" (nf benchDrg1 drgCmd6), -- 3.93 μs ± 210 ns | 4.92 μs ± 441 ns
        bench "trace2_dragon_6" (nf benchDrg2 drgCmd6)  -- 5.03 μs ± 426 ns | 3.69 μs ± 271 ns
      ],                                                --                  |
      bgroup "canopy_3" [                               --                  |
        bench "trace1_dragon_3" (nf benchDrg1 drgCmd3), -- 478  ns ±  28 ns | 569  ns ±  55 ns
        bench "trace2_dragon_3" (nf benchDrg2 drgCmd3)  -- 533  ns ±  28 ns | 460  ns ±  30 ns
      ],                                                --                  |
      bgroup "canopy_1" [                               --                  |
        bench "trace1_dragon_1" (nf benchDrg1 drgCmd1), -- 99.3 ns ± 6.5 ns | 117  ns ± 6.6 ns
        bench "trace2_dragon_1" (nf benchDrg2 drgCmd1)  -- 114  ns ± 7.3 ns | 94.8 ns ± 7.0 ns
      ]
    ]
  ]
  where canCmd8 = expandLSystem canopy 8
        canCmd6 = expandLSystem canopy 6
        canCmd3 = expandLSystem canopy 3
        canCmd1 = expandLSystem canopy 1
        canAng  = angle canopy
        drgCmd6 = expandLSystem dragon 6
        drgCmd3 = expandLSystem dragon 3
        drgCmd1 = expandLSystem dragon 1
        drgAng  = angle dragon
        benchCan1 cmds = trace1 cmds canAng Blue
        benchCan2 cmds = trace2 cmds canAng Blue
        benchDrg1 cmds = trace1 cmds drgAng Blue
        benchDrg2 cmds = trace2 cmds drgAng Blue
```

The benchmarking results are documented below:
```
RESULTS AFTER REFACTORING TRACES (THURSDAY)
After refactoring the trace functions in an effort to improve
their performance, the speed of both functions increased tremendously
for higher n like n=6, especially in the case of trace2. However for
lower n we see negligible or negative performance boosts for trace1.

Observe that in this version, trace2 is now faster than trace1. 
I theorize that this is because of the non-tail-recursive nature of
trace1 for dealing with F commands, while the equivalent pattern match
clause is tail-recursive in trace2.

-----------------------------

ANALYSIS OF RESULTS BEFORE REFACTORING TRACES (WEDNESDAY)

Focus on canopy_3 for instance: 
      trace1_canopy_3: OK
        34.1 μs ± 1.9 μs
      trace2_canopy_3: OK
        38.2 μs ± 3.7 μs

Here we see that trace1 (the recurse-based implementation)
benches 112% more quickly than trace2 (stack-based).

This came as a surprise at first, since trace1 uses the (somewhat ugly)
++ operator requiring traversal of the left lists (inefficient), 
whilst I expected trace2 to run in linear time.

I conjecture that this is due to the following:
  > Although ++ is usually inefficient, its use in trace1 means
    it specifically traverses the inner commands (contents of the B 
    constructor). These lists could be small enough that the time
    cost of ++ does not dominate the bench result of trace1.
  > Meanwhile, trace2 utilizes reverse on the final list of ColoredLines
    as the last step in computation, which could have a large constant factor 
    attached since this final list would get "exponentially" larger for higher 
    iterations in ExpandLSystem.
    (depending on how many branches B [Command] are within cmds)

This conclusion is supported by studying how the two trace functions
performed when the number of expansion steps was varied.

When n=6 (see canopy_6 above), meaning there were more expansion steps and hence 
increased branching from any Bs arising from the canopy LSystem, we see that 
trace1 performs 228% faster than trace2, higher than the 112% difference 
seen for n=3.

Conversely, looking at the case where n=1, the difference in performance is
effectively none*. For small n such as this, the costly ++ operation in trace1
starts to contribute significantly to its overall time cost, while trace2's
constant factor (referring to its supposedly improved time-complexity) becomes 
smaller due to having to reverse a shorter list of ColoredLines.

Similar results can be concluded from the dragon benchmarks, where trace1
performs better than trace2, with the difference in speed increasing for larger
n and becoming negligible for smaller n.

*NOTE. in some runs of `cabal bench`, trace1 still performs faster
than trace2 for n=1 but with marginal difference.

As for experimentation, I tried to improve the speed of the trace functions
using the strict bang (!) pattern, but this led to either similar results
(like !acc in trace2) or that which is way worse (like my attempts in trace1).
```
