{-  This file is part of hs-panelmethod.

    hs-panelmethod is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    hs-panelmethod is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with hs-panelmethod.  If not, see <http://www.gnu.org/licenses/>. -}

module Main where

import Control.Monad (unless)
import Graphics.UI.GLUT
import Data.Foldable (foldl')
import qualified Graphics.GLUtil as U
import Data.List (find, partition)
import Data.Maybe (catMaybes)
import Data.IORef
import Linear
import qualified Numeric.LinearAlgebra as LA
import GHC.Float (float2Double, double2Float)

type MyLine = (V2 Float, V2 Float)

data Shaders = Shaders {  vertexShader :: Shader
                        , fragmentShader :: Shader
                        , program :: Program
                        , vertexPosition :: AttribLocation
                        , transform :: UniformLocation
                        }

data Resources = Resources {  vertexBuffer :: BufferObject
                            , shaders :: Shaders
                            , lineList :: [MyLine]
                            , lineCount :: GLint
                            , partialLine :: Maybe (V2 Float)
                            , mousePos :: V2 Float
                            , winSize :: Size
                            , stream :: [(MyLine,Float)]
                            , streamVectorCount :: GLint
                            , gridSize :: Float
                            , polygons :: [[MyLine]]
                            }

initVertexBuffer :: IO BufferObject
initVertexBuffer = U.makeBuffer ArrayBuffer ([] :: [GLfloat])

initShaders :: IO Shaders
initShaders = do
        vs <- U.loadShader VertexShader "vert.glsl"
        fs <- U.loadShader FragmentShader "frag.glsl"
        p <- U.linkShaderProgram [vs,fs]
        vpos <- get (attribLocation p "in_Position")
        mvp <- get (uniformLocation p "transform")
        return Shaders { vertexShader = vs
                         , fragmentShader = fs
                         , program = p
                         , vertexPosition = vpos
                         , transform = mvp }


initResources :: IO Resources
initResources = do
    vbuf <- initVertexBuffer
    s <- initShaders
    return Resources { vertexBuffer = vbuf
                     , shaders = s
                     , lineList = []
                     , lineCount = 0
                     , partialLine = Nothing
                     , mousePos = V2 0 0
                     , winSize = Size 500 500
                     , stream = []
                     , streamVectorCount = 0
                     , gridSize = 25
                     , polygons = []}

main :: IO ()
main = do
    (_progName, _args) <- getArgsAndInitialize
    initialDisplayMode $= [DoubleBuffered, Multisampling]
    initialWindowSize $= Size 500 500
    _window <- createWindow "Source panel"
    clientState VertexArray $= Enabled
    res <- initResources
    resRef <- newIORef res
    displayCallback $= display resRef
    reshapeCallback $= Just (reshape resRef)
    keyboardMouseCallback $= Just (input resRef)
    passiveMotionCallback $= Just (motion resRef)
    mainLoop

display :: IORef Resources -> DisplayCallback
display _res = do
    r <- get _res
    let lc = lineCount r
        vc = streamVectorCount r
        shader = shaders r
        pos = vertexPosition shader
        transformMatrix = transform shader
        prog = program shader
        size@(Size width height) = winSize r
        fx = fromIntegral width
        fy = fromIntegral height
        vad = VertexArrayDescriptor 2 Float 0 U.offset0
    clearColor $= Color4 0 0 0.3 1
    clear [ ColorBuffer ]
    viewport $= (Position 0 0, size)
    currentProgram $= Just prog
    let mvp = getOrtho 0 fx 0 fy
    U.asUniform mvp transformMatrix
    vertexAttribArray pos $= Enabled
    vertexAttribPointer pos $= (ToFloat, vad)
    bindBuffer ArrayBuffer $= Just (vertexBuffer r)
    drawArrays Lines 0 (2*(vc+lc))
    swapBuffers

reshape :: IORef Resources -> ReshapeCallback
reshape _res size =
    do
        _res $~! (\x -> x { winSize = size })
        updateFlow _res
        postRedisplay Nothing

motion :: IORef Resources -> MotionCallback
motion _res (Position x y) =
    do
        _res $~! (\r -> r {mousePos = V2 (fromIntegral x) (fromIntegral y)})
        return ()

input :: IORef Resources -> KeyboardMouseCallback
input _res _key Down _modifiers p =
    do
        r <- get _res
        case _key of
            MouseButton LeftButton  -> click _res (translateGLUTCoords (winSize r) p)
            _                       -> return ()
input _ _ _ _ _ = return ()

translateGLUTCoords :: Size -> Position -> V2 Float
translateGLUTCoords (Size _ height) (Position x y) = V2 (fromIntegral x) (fromIntegral $ height - y)

findEndpoint :: V2 Float -> [MyLine] -> Maybe (V2 Float)
findEndpoint p = find (\v -> qd v p < 100) . foldr (\(x,y) acc-> x:y:acc) []

findClosedPolygon :: [MyLine] -> ([MyLine], [MyLine])
findClosedPolygon ls = ([],ls) --(connected,rest)
    where
    (connected,rest) = partition (\(v1,v2)->
        elem v1 intersectionPoints && elem v2 intersectionPoints) ls
    intersectionPoints = foldl' (\acc (_,v1)->
        case find (\(v2,_) -> v1 == v2) ls of
            Nothing -> acc
            Just _  -> v1:acc) [] ls

click :: IORef Resources -> V2 Float -> IO ()
click _res _position = do
    r <- get _res
    let ls = lineList r
        lc = lineCount r
        pl = partialLine r
        polys = polygons r
        existing = findEndpoint _position ls
        p = head $ catMaybes (existing : [Just _position])
    case pl of
        Nothing ->
            _res $~! (\x -> x { partialLine = Just p } )
        Just pos ->
            unless (pos == p) $
                do
                let newLines = (pos,p) : ls
                    pair = findClosedPolygon newLines
                case pair of
                    ([],_)         ->
                        _res $~! (\x ->
                            x { lineList = newLines
                              , lineCount = lc + 1
                              , partialLine = Nothing } )
                    (polygon,rest) ->
                        _res $~! (\x ->
                            x { lineList = rest
                              , lineCount = toEnum $ length rest
                              , polygons = polygon:polys
                              , partialLine = Nothing} )
                -- print pair
                updateFlow _res
                postRedisplay Nothing

updateFlow :: IORef Resources -> IO ()
updateFlow _res = do
    r <- get _res
    let ls = lineList r
        Size width height = winSize r
        newStream = calculateStream ls
        gs = gridSize r
        visualization = visualizeStream (0, fromIntegral width, 0, fromIntegral height, gs) newStream
    vb <- U.makeBuffer ArrayBuffer (serialize $ ls ++ visualization :: [GLfloat])
    _res $~! (\x -> x { stream = newStream
                      , vertexBuffer = vb
                      , streamVectorCount = toEnum $ length visualization} )
    where
    serialize = concatMap devector . concatMap deTuple
    devector (V2 x y) = [x,y]
    deTuple (x,y) = [x,y]

foldingFunction :: Eq a => (a,MyLine) -> (a,MyLine) -> [Float] -> [Float]
foldingFunction (i,controlPoint) (j,panel) acc
    | i == j    = 0.5 : acc
    | otherwise = dot n inducedVelocity / (2*pi) : acc
    where
    inducedVelocity = integralCoefficient (getMidpoint controlPoint) panel
    n = getNormal controlPoint

calculateStream :: [MyLine] -> [(MyLine,Float)]
calculateStream ls =
    let numberedList = zip [(0::Int)..] ls
        equations = map (\z@(_,target) ->
            (foldr (foldingFunction z) [] numberedList, -freestreamStrength target)
            ) numberedList
        leftHandSide = map (map float2Double . fst) equations
        rightHandSide = map (float2Double . snd) equations
        n = length rightHandSide
        strengths = LA.linearSolve (LA.fromLists leftHandSide :: LA.Matrix LA.R)
                                   ((n LA.>< 1) rightHandSide) in
        case strengths of
            Nothing -> []
            Just s  -> zip ls (map double2Float . concat $ LA.toLists s)

getOrtho :: Float -> Float -> Float -> Float -> M44 GLfloat
getOrtho left right bottom top =
    V4  (V4 a   0.0 0.0 tx)
        (V4 0.0 b   0.0 ty)
        (V4 0.0 0.0 c   tz)
        (V4 0.0 0.0 0.0 1.0)
    where
    a = 2.0/(right-left)
    b = 2.0/(top-bottom)
    c = -2.0/(far-near)
    far = 2.0
    near = 0.0
    tx = - (right + left)/(right - left)
    ty = - (top + bottom)/(top - bottom)
    tz = - (far + near)/(far - near)

getNormal :: MyLine -> V2 Float
getNormal (p1, p2) = Linear.normalize $ V2 (-dy) dx
    where
    V2 dx dy = p2 - p1

getMidpoint :: MyLine -> V2 Float
getMidpoint (v1, v2)  = 0.5 * (v1 + v2)

visualizeStream :: (Float, Float, Float, Float, Float) -> [(MyLine,Float)] -> [(V2 Float,V2 Float)]
visualizeStream (left, right, bottom, top, delta) sources = zip coords (zipWith (+) coords normalizedField)
    where
    scalingFactor = 1.3 * delta/(2 * norm freestreamVelocity)
    normalizedField = map (scalingFactor *^) velocityField
    velocityField = map (totalVelocity sources) coords
    coords = [V2 x y | x <- [left,left+delta..right], y <- [bottom,bottom+delta..top]]

freestreamVelocity :: V2 Float
freestreamVelocity = V2 25.0 0.0

freestreamStrength :: MyLine -> Float
freestreamStrength l = getNormal l `dot` freestreamVelocity

integralCoefficient :: V2 Float -> MyLine -> V2 Float
integralCoefficient target panel = foldl' integration (V2 0 0) chunks
    where
    integration acc v = acc + (target - v) ^* (ds / qd target v)
    (chunks,ds) = discretize panel subDivisions
    subDivisions = 100

discretize :: MyLine -> Float -> ([V2 Float],Float)
discretize (p1, p2) count = (map (\n -> p1 + dds * fromInteger n) [0..round count], nds)
    where
    ds = p2 - p1
    dds = ds ^/ count
    nds = norm dds

totalVelocity :: [(MyLine,Float)] -> V2 Float -> V2 Float
totalVelocity sources p2 = sourceContrib + freestreamVelocity
    where
    sourceContrib = foldl' (\acc panel-> acc + velocityContribAt p2 panel) (V2 0 0) sources

velocityContribAt :: V2 Float -> (MyLine,Float) -> V2 Float
velocityContribAt target (panel,strength) = integralCoefficient target panel ^* (strength/(2*pi))
