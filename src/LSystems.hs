module LSystems ( LSystem(LSystem), ColouredLine, Command(..)
                , angle, axiom, rules, lookupChar
                , expandOne, expand, move, parse, trace1, trace2, traceHS
                , expandLSystem ) where

import IC.Colour
import Graphics.UI.GLUT (syncGpuCommandsComplete)

import Data.Fixed (mod')
import Data.List (foldl')

-- Part 1 - Warmup
----------------------------------------------------------
type Rules a = [(Char, [a])]
data LSystem = LSystem Float [Char] (Rules Char)

-- Functions for working wirotsystems. Skeleton optional...

-- Returns the rotation angle for the given system.
angle :: LSystem -> Float
angle (LSystem rot _ _) = rot

-- Returns the axiom string for the given system.
axiom :: LSystem -> [Char]
axiom (LSystem _ ax _) = ax

-- Returns the set of rules for the given system.
rules :: LSystem -> Rules Char
rules (LSystem _ _ rs) = rs

--
-- Pre: the character has a binding in the Rules list
--
lookupChar :: Rules a -> Char -> [a]
lookupChar rs c = snd . head $ filter (\(k,_) -> k == c) rs
-- -- alternate solution wirotlist compre
-- lookupChar [] _ = []
-- lookupChar rs c = head [v | (k,v) <- rs, k == c]

-- Part 2: Expansion and Parsing
data Command = F | L | R | B [Command] deriving Show

--
-- Expand command string s once using rule table r
--
expandOne :: Rules Char -> [Char] -> [Char]
expandOne = concatMap . lookupChar

--
-- Expand command string s n times using rule table r
--
expand :: Rules Char -> [Char] -> Int -> [Char]
-- expand rs ax n = foldr expandOne ax $ replicate n rs 
expand rs ax = (!!) $ iterate (expandOne rs) ax

parse :: [Char] -> [Command]
parse cmds = reverse . fst $ go ([], cmds)
    where
        go :: ([Command], [Char]) -> ([Command], [Char])
        go (stk, ']':cmds) = (stk, cmds)
        go (stk, '[':cmds) = let (inner, rest) = go ([], cmds) in go (B (reverse inner) : stk, rest)
        go (stk, cmd:cmds) = go (lookupChar commandMap cmd ++ stk, cmds)
        go (stk, _)        = (stk, [])

expandLSystem :: LSystem -> Int -> [Command]
expandLSystem lsys = parse . expand rs ax
    where rs = rules lsys
          ax = axiom lsys
-- -- alternative solution without rules and axiom
-- expandLSystem (LSystem _ ax rs) = parse . expand rs ax

-- Part 3: Turtles
type Vertex = (Float, Float)
type TurtleState = (Vertex, Float)
type ColouredLine = (Vertex, Vertex, Colour)

initialState :: TurtleState
initialState = ((0, 0), 90)

-- Move a turtle.
--
-- F moves distance 1 in the current direction.
-- L rotates left according to the given angle.
-- R rotates right according to the given angle.
move :: Command -> Float -> TurtleState -> TurtleState
move L rot (pos,ang)   = (pos, ang+rot)
move R rot (pos,ang)   = (pos, ang-rot)
move F _   ((x,y),ang) = ((x + cos rad, y + sin rad), ang)
    where rad = degreesToRadians ang

trace1 :: [Command] -> Float -> Colour -> [ColouredLine]
trace1 cmds rot col = go initialState cmds
    where
        go :: TurtleState -> [Command] -> [ColouredLine]
        go st         ((B inner):cmds) = go st inner ++ go st cmds
        go st@(pos,_) (F:cmds)         = let st'@(pos',_) = move F rot st
                                         in  (pos,pos',col) : go st' cmds
        go st         (cmd:cmds)       = go (move cmd rot st) cmds
        go _ _ = []

-- This version uses an explicit stack of residual commands and turtle states
trace2 :: [Command] -> Float -> Colour -> [ColouredLine]
trace2 cmds rot col = go initialState cmds []
    where
        go :: TurtleState -> [Command] -> [ColouredLine] -> [ColouredLine]
        go st         ((B inner):cmds) stk = go st inner [] ++ go st cmds stk
        go st@(pos,_) (F:cmds)         stk = let st'@(pos',_) = move F rot st
                                             in  go st' cmds $ (pos,pos',col) : stk
        go st         (cmd:cmds)       stk = go (move cmd rot st) cmds stk
        go _ [] stk = stk

------------------------------------------------------------------------------
-- Expansion

-- TurtleState, but also with the pen color the turtle is holding down
type TurtleState' = (TurtleState, Colour)

-- Like trace2, but with hue shifting (HS)
traceHS :: [Command]      -- ^ Command list
        -> Float          -- ^ Degrees to turn on L/R
        -> Colour         -- ^ Initial color of the turtle
        -> Float          -- ^ Angle (in degrees) to hue-shift the pen on each line
        -> [ColouredLine] -- ^ Output (list of lines to draw)
traceHS cmds rot col deg = go (initialState,col) cmds []
    where
        sft = degreesToRadians deg

        go :: TurtleState' -> [Command] -> [ColouredLine] -> [ColouredLine]
        go st                  ((B inner):cmds) stk = go st inner [] ++ go st cmds stk
        go st@(turt@(pos,_),c) (F:cmds)         stk = let turt'@(pos',_) = move F rot turt
                                                in  go (turt',hueRot c sft) cmds $ (pos,pos',c) : stk
        go st@(turt,c)         (cmd:cmds)       stk = go (move cmd rot turt,c) cmds stk
        go _ [] stk = stk
{-
Use the function drawLSystemHS to render the lines generated in this function!

Cool examples to try out:
drawLSystemHS False canopy    6  (RGB (30/255) (176/255) (25/255))   1
drawLSystemHS True  arrowHead 7  (RGB (240/255) (152/255) (115/255)) 0.07
drawLSystemHS False dragon    16 Green                                  (-0.2)
drawLSystemHS False tree      12 Green                                  0.15

Note that 
    drawLSystemHS live lsys n col 0
does the same thing as
    drawLSystem1 live lsys n col
-}

-- Provided Helper Functions
------------------------------------------------------------------------------

degreesToRadians :: Float -> Float
degreesToRadians x = (x / 180) * pi

commandMap :: Rules Command
commandMap = [ ('M', [F])
             , ('N', [F])
             , ('X', [])
             , ('Y', [])
             , ('A', [])
             , ('+', [L])
             , ('-', [R])
             ]
