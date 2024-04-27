{-# LANGUAGE OverloadedRecordDot #-}

module Main where

import Control.Monad (guard)
import Data.Fixed (mod')
import Data.List (sortOn)
import Data.Maybe (isJust, fromMaybe, listToMaybe)
import GHC.Float (float2Double)
import Linear (V2 (..), Metric (distance))
import Linear.Metric (dot, signorm)
import Linear.Vector ((^*))
import Raylib.Core (initWindow, beginDrawing, endDrawing, windowShouldClose, clearBackground, setConfigFlags, getMousePosition, isKeyPressed)
import Raylib.Core.Shapes (drawRectangleLinesEx, drawRectangleRec, drawLine)
import Raylib.Types (Vector2(..), ConfigFlag (VsyncHint), Rectangle (Rectangle, rectangle'x, rectangle'y, rectangle'width, rectangle'height), Color (Color), KeyboardKey (..))
import Raylib.Util (WindowResources)
import Raylib.Util.Colors (white, black, yellow, gray)
import Raylib.Core.Text (drawText)
import FractionalOps (intersectSegSeg)

-- ( ( CellXi, CellYi ), AngleDeg )
type Mirror = ((Float, Float), Float)

type Line a = (V2 a, V2 a)

data State = State
    { wr :: WindowResources
    , ad :: Double
    , mv :: V2 Double
    , fixed :: Maybe (V2 Double) }

fi :: (Integral a, Num b) => a -> b
fi = fromIntegral

f2d :: Float -> Double
f2d = float2Double

rads :: Floating a => a -> a
rads d = d * (pi / 180)

v2Rl :: Vector2 -> V2 Double
v2Rl v = V2 (f2d v.vector2'x) (f2d v.vector2'y)

-- Take V2s and get around bugged line rendering in raylib
drawLineV' :: (RealFrac a) => Color -> V2 a -> V2 a -> IO ()
drawLineV' c (V2 ax ay) (V2 bx by) = let
    m = (by - ay) / (bx - ax)
    q = ay - m * ax
    (bx', by') = if bx < 0 then (0, q)
        else if by < 0 then ((-q) / m, 0)
        else if bx >= fi width then (fi width, m * (fi width) + q)
        else if by >= fi height then ((fi height - q) / m, fi height)
        else (bx, by)
    in drawLine (round ax) (round ay) (round bx') (round by') c

-- Native implementation of raylib vector2Reflect
vector2Reflect' :: (Fractional a) => V2 a -> V2 a -> V2 a
vector2Reflect' v@(V2 vx vy) n@(V2 nx ny) = V2 (vx - (2 * nx * dot')) (vy - (2 * ny * dot'))
    where dot' = v `dot` n

laserLengthPx :: (Fractional a) => a
laserLengthPx = 10000

gridSize :: Int
gridSize = 17

cellSizePx :: (Fractional a) => a
cellSizePx = 50

mirrorSizePx :: (Fractional a) => a
mirrorSizePx = cellSizePx * 0.8

gridSizePx :: (Fractional a) => a
gridSizePx = fi gridSize * cellSizePx

gridPadPx :: (Fractional a) => a
gridPadPx = cellSizePx

pad :: (Fractional a) => a -> a
pad = (+) gridPadPx

padR :: Rectangle -> Rectangle
padR r = r
    { rectangle'x = pad r.rectangle'x
    , rectangle'y = pad r.rectangle'y }

padV :: (Fractional a) => V2 a -> V2 a
padV = (+) gridPadPx

padL :: (Fractional a) => Line a -> Line a
padL (va, vb) = (padV va, padV vb)

width :: Int
width = round $ (gridSizePx :: Float) + (2 * (gridPadPx :: Float))

height :: Int
height = width

mirrors :: [Mirror]
mirrors =
    [ ( ( 8  ,  0  ), -22.5 )
    , ( (16  ,  0  ),  45   )
    , ( ( 0  ,  2  ), 135   )
    , ( ( 2  ,  2  ),  22.5 )
    , ( ( 6  ,  2  ), -67.5 )
    , ( (10  ,  2  ),  22.5 )
    , ( (12  ,  2  ), 135   )
    , ( (14  ,  2  ),  22.5 )
    , ( ( 0  ,  4  ),  67.5 )
    , ( ( 4  ,  4  ),  22.5 )
    , ( ( 6  ,  4  ), 135   )
    , ( ( 8  ,  4  ), -67.5 )
    , ( ( 8.5,  4.5), -67.5 )
    , ( (10  ,  4  ),  45   )
    , ( (12  ,  4  ), 135   )
    , ( (16  ,  4  ), -22.5 )
    , ( ( 0  ,  6  ), 135   )
    , ( ( 2  ,  6  ), -67.5 )
    , ( ( 6  ,  6  ),   0   )
    , ( ( 8  ,  6  ),  45   )
    , ( (10  ,  6  ),  22.5 )
    , ( (14  ,  6  ), -22.5 )
    , ( ( 4  ,  8  ), -67.5 )
    , ( ( 8  ,  8  ),  90   )
    , ( (12.2,  7.8),  45   )
    , ( (16  ,  8  ),  67.5 )
    , ( ( 0  , 10  ),  45   )
    , ( ( 4  , 10  ), 135   )
    , ( ( 6  , 10  ), -67.5 )
    , ( (10  , 10  ), -67.5 )
    , ( (14  , 10  ), -22.5 )
    , ( ( 0  , 12  ), -67.5 )
    , ( ( 6  , 12  ), 135   )
    , ( (10  , 12  ),  45   )
    , ( (16  , 12  ), 135   )
    , ( ( 2  , 14  ),  90   )
    , ( ( 6  , 14  ),   0   )
    , ( (10  , 14  ), -22.5 )
    , ( (12  , 14  ), 135   )
    , ( (14  , 14  ),  22.5 )
    , ( ( 0  , 16  ),  67.5 )
    , ( ( 4  , 16  ), -22.5 )
    , ( ( 8  , 16  ),  22.5 )
    , ( (10  , 16  ), 135   )
    , ( (12  , 16  ), -22.5 )
    , ( (16.2, 16.2), 135   ) ]

mirrorLine :: Mirror -> Line Double
mirrorLine ((xi, yi), ad') = let
    c = V2
        ( (f2d xi) * cellSizePx + (cellSizePx / 2) )
        ( (f2d yi) * cellSizePx + (cellSizePx / 2) )
    va = V2
        ( mirrorSizePx / 2 * (cos $ rads $ f2d ad') )
        ( mirrorSizePx / 2 * (sin $ rads $ f2d ad') )
    vb = V2
        ( mirrorSizePx / 2 * (cos $ rads $ f2d (ad' - 180)) )
        ( mirrorSizePx / 2 * (sin $ rads $ f2d (ad' - 180)) )
    in ( c + va, c + vb )

mirrorLines :: [Line Double]
mirrorLines = map (padL . mirrorLine) mirrors

gridSquare :: Int -> Int -> Rectangle
gridSquare xi yi = padR $ Rectangle
    { rectangle'x      = fi xi * cellSizePx
    , rectangle'y      = fi yi * cellSizePx
    , rectangle'width  = cellSizePx
    , rectangle'height = cellSizePx }

gridRow :: Int -> Rectangle
gridRow yi = padR $ Rectangle
    { rectangle'x      = 0
    , rectangle'y      = fi yi * cellSizePx
    , rectangle'width  = fi gridSize * cellSizePx
    , rectangle'height = cellSizePx }

gridCol :: Int -> Rectangle
gridCol xi = padR $ Rectangle
    { rectangle'x      = fi xi * cellSizePx
    , rectangle'y      = 0
    , rectangle'width  = cellSizePx
    , rectangle'height = fi gridSize * cellSizePx }

drawGrid :: IO ()
drawGrid = do
    flip mapM_ [0,4..(gridSize - 1)] $ \xi -> drawRectangleRec (gridCol xi) $ Color 25 25 25 255
    flip mapM_ [0,4..(gridSize - 1)] $ \yi -> drawRectangleRec (gridRow yi) $ Color 25 25 25 255
    flip mapM_ [0,4..(gridSize - 1)] $ \xi -> flip mapM_ [0,4..(gridSize - 1)] $ \yi -> drawRectangleRec (gridSquare xi yi) $ Color 50 50 50 255
    flip mapM_ [0..  (gridSize - 1)] $ \xi -> flip mapM_ [0..  (gridSize - 1)] $ \yi -> drawRectangleLinesEx (gridSquare xi yi) 1 $ Color 75 75 75 255

drawMirror :: Line Double -> IO ()
drawMirror (va, vb) = drawLineV' white va vb

drawMirrors :: IO ()
drawMirrors = mapM_ drawMirror mirrorLines

trackLaser :: Double -> Line Double -> [Line Double]
trackLaser l (va, vb) = let
    intersections = filter (\(_, i) -> (abs $ distance va i) > 0.1) $ sortOn (\(_, i) -> distance va i)
        [ (m, i) | (m, Just i) <- map (\m@(ma, mb) -> (m, intersectSegSeg va vb ma mb)) mirrorLines ]
    in case (listToMaybe intersections) of
        Nothing -> [(va, vb)]
        (Just (((V2 ma_x ma_y), (V2 mb_x mb_y)), i)) -> let
            normal = signorm $ V2 (ma_y - mb_y) (mb_x - ma_x)
            r = i + ((signorm $ vector2Reflect' (vb - va) normal) ^* (l - (distance va i)))
            in (va, i):(trackLaser (l - (distance va i)) (i, r))

drawLaser :: State -> IO ()
drawLaser s = let
    va = fromMaybe s.mv s.fixed
    vb = va + ((V2 (cos $ rads s.ad) (sin $ rads s.ad)) * laserLengthPx)
    tracks = trackLaser laserLengthPx (va, vb)
    in flip mapM_ tracks $ uncurry (drawLineV' yellow)

drawHint :: IO ()
drawHint =
    drawText
        "[Q,E]: Rotate | [Space]: Freeze"
        (round $ (cellSizePx :: Float))
        (round $ ((cellSizePx :: Float) - 20) / 2)
        20
        gray

input :: State -> IO State
input s = do
    mv' <- getMousePosition
    ePressed <- isKeyPressed KeyE
    qPressed <- isKeyPressed KeyQ
    let ad' = if (isJust s.fixed) then s.ad
        else if ePressed then (s.ad + 22.5) `mod'` 360
        else if qPressed then (s.ad - 22.5) `mod'` (-360)
        else s.ad
    spacePressed <- isKeyPressed KeySpace
    let fixed' = if (spacePressed && isJust s.fixed) then Nothing
        else if (spacePressed) then (Just $ v2Rl mv')
        else s.fixed
    pure s
        { ad = ad'
        , mv = v2Rl mv'
        , fixed = fixed' }

draw :: State -> IO ()
draw s = do
    beginDrawing >> do
        clearBackground black
        drawGrid
        drawMirrors
        drawHint
        drawLaser s
    endDrawing

loop :: State -> IO ()
loop _s = do
    guard =<< not <$> windowShouldClose
    input _s >>= \s -> draw s >> loop s

main :: IO ()
main = do
    setConfigFlags [VsyncHint]
    wr' <- initWindow width height "Hall of Mirrors 2"
    loop $ State
        { wr = wr'
        , ad = 0
        , mv = V2 0 0
        , fixed = Nothing }
