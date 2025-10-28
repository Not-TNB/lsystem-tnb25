module IC.Colour where

import Data.Fixed(mod')
import Graphics.UI.GLUT hiding (Vertex, Red, Blue, Green, RGB)

data Colour = Black | White
            | Red | Green | Blue
            | Cyan | Magenta | Yellow
            | RGB Float Float Float
            | HSV Float Float Float
            deriving (Show, Eq, Ord)

-- Moved from Graphics.hs to prevent circular dependencies
toColor3 :: Colour -> Color3 GLfloat
toColor3 Black   = Color3 0 0 0
toColor3 White   = Color3 1 1 1
toColor3 Red     = Color3 1 0 0
toColor3 Green   = Color3 0 1 0
toColor3 Blue    = Color3 0 0 1
toColor3 Cyan    = Color3 0 1 1
toColor3 Magenta = Color3 1 0 1
toColor3 Yellow  = Color3 1 1 0
toColor3 (RGB r g b) = Color3 r g b
toColor3 (HSV h s v) = Color3 r g b
  where RGB r g b = toRGB (HSV h s v)

-- | Convert RGB to HSV
toHSV :: Colour -> Colour
toHSV (RGB r g b) = HSV h s cMax
    where cMax = max r (max g b)
          cMin = min r (min g b)
          delta = cMax - cMin
          h' | delta == 0 = 0
             | cMax == r  = ((g - b) / delta) `mod'` 6
             | cMax == g  = ((b - r) / delta) + 2
             | otherwise  = ((r - g) / delta) + 4
          h = (h' / 6) `mod'` 1
          s | cMax == 0 = 0
            | otherwise = delta / cMax
toHSV (HSV h s v) = HSV h s v
toHSV col = toHSV (RGB r g b)
  where (Color3 r g b) = toColor3 col

-- | Convert HSV to RGB
toRGB :: Colour -> Colour
toRGB (HSV h s v) = RGB (r'+m) (g'+m) (b'+m)
    where c = v * s
          x = c * (1 - abs ((h * 6) `mod'` 2 - 1))
          m = v - c
          (r',g',b')
            | 0 <= h && h < 1/6   = (c,x,0)
            | 1/6 <= h && h < 2/6 = (x,c,0)
            | 2/6 <= h && h < 3/6 = (0,c,x)
            | 3/6 <= h && h < 4/6 = (0,x,c)
            | 4/6 <= h && h < 5/6 = (x,0,c)
            | otherwise           = (c,0,x)
toRGB (RGB r g b) = RGB r g b
toRGB col = RGB r g b
  where (Color3 r g b) = toColor3 col

-- | Rotate hue
hueRot :: Colour -> Float -> Colour
hueRot (HSV h s v) theta = HSV ((h + theta / (2 * pi)) `mod'` 1) s v
hueRot col theta         = HSV ((h + theta / (2 * pi)) `mod'` 1) s v
    where HSV h s v = toHSV col