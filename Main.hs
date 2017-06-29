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

import Control.Monad (unless,when)
import Control.Parallel.Strategies (parMap,rdeepseq)
import Data.Foldable (foldl')
import Data.IORef
import Data.List (find, partition)
import Data.Maybe (catMaybes,fromJust)
import Foreign
import Foreign.C.String
import GHC.Float (float2Double, double2Float)
import Graphics.GL
import Graphics.UI.GLFW
import Linear
import Numeric.LinearAlgebra (linearSolve, fromLists, toLists, Matrix, R, (><))
import System.IO
import VectorUtils

type Panel = (Line,Float)

data Shaders = Shaders {  vertexShader :: GLuint
                        , fragmentShader :: GLuint
                        , program :: GLuint
                        , vertexPosition :: GLuint
                        , panelBlockIndex :: GLuint
                        , transformUniform :: GLint
                        , numPanelsUniform :: GLint
                        , freestreamUniform :: GLint
                        }

data Resources = Resources {  lineBuffer :: GLuint
                            , quadBuffer :: GLuint
                            , panelBuffer :: GLuint
                            , shaders :: Shaders
                            , lineList :: [Line]
                            , lineCount :: GLint
                            , partialLine :: Maybe FV2
                            , mousePos :: FV2
                            , winSize :: (Int,Int)
                            , panels :: [Panel]
                            , panelCount :: GLint
                            , gridSize :: Float
                            , polygons :: [[Line]]
                            }

printGLErrors :: IO ()
printGLErrors = do
    errorCode <- glGetError
    when (errorCode /= GL_NO_ERROR) $
        do
        putStr "OpenGL Error: "
        print errorCode
        printGLErrors

initBuffer :: IO GLuint
initBuffer = alloca $ \result ->
    do
    putStrLn "Initializing buffer"
    glCreateBuffers 1 result
    printGLErrors
    peek result

loadShader :: GLenum -> String -> IO GLuint
loadShader shaderType path = withFile path ReadMode $ \h->
    do
    putStrLn "Loading shader"
    text <- hGetContents h
    s <- glCreateShader shaderType
    printGLErrors
    withCString text $ \str->
        alloca $ \strp -> do
            poke strp str
            glShaderSource s (toEnum $ length text) strp nullPtr
            printGLErrors
    glCompileShader s
    printGLErrors
    return s

initShaders :: IO Shaders
initShaders = do
    vs <- loadShader GL_VERTEX_SHADER "vert.glsl"
    fs <- loadShader GL_FRAGMENT_SHADER "frag.glsl"
    putStrLn "Creating shader program"
    p <- glCreateProgram
    printGLErrors
    putStrLn "Attaching shaders"
    glAttachShader p vs
    printGLErrors
    glAttachShader p fs
    printGLErrors
    putStrLn "Binding attribute location"
    withCString "in_Position" $ \str-> glBindAttribLocation p 0 str
    printGLErrors
    putStrLn "Linking program"
    glLinkProgram p
    printGLErrors
    putStrLn "Getting uniforms"
    tu <- withCString "transform" $ \str-> glGetUniformLocation p str
    printGLErrors
    npu <- withCString "numPanels" $ \str-> glGetUniformLocation p str
    printGLErrors
    fsu <- withCString "freestream" $ \str-> glGetUniformLocation p str
    printGLErrors
    pbi <- withCString "panels" $ \str-> glGetUniformBlockIndex p str
    printGLErrors
    putStrLn "Binding uniform block"
    glUniformBlockBinding p pbi 2
    printGLErrors
    return Shaders { vertexShader = vs
                   , fragmentShader = fs
                   , program = p
                   , vertexPosition = 0
                   , panelBlockIndex = pbi
                   , transformUniform = tu
                   , numPanelsUniform = npu
                   , freestreamUniform  = fsu }

initResources :: IO Resources
initResources = do
    lbuf <- initBuffer
    qbuf <- initBuffer
    pbuf <- initBuffer
    s <- initShaders
    return Resources { lineBuffer = lbuf
                     , quadBuffer = qbuf
                     , panelBuffer = pbuf
                     , shaders = s
                     , lineList = []
                     , lineCount = 0
                     , partialLine = Nothing
                     , mousePos = V2 0 0
                     , winSize = (500,500)
                     , panels = []
                     , panelCount = 0
                     , gridSize = 15
                     , polygons = [] }

main :: IO ()
main = do
    success <- Graphics.UI.GLFW.init
    when success $ do
        setErrorCallback (Just errorCallback)
        win <- fromJust <$> createWindow 500 500 "Panel method" Nothing Nothing
        res <- initResources
        resRef <- newIORef res
        makeContextCurrent (Just win)
        setWindowRefreshCallback win $ Just (display resRef)
        setWindowSizeCallback win $ Just (reshape resRef)
        setMouseButtonCallback win $ Just (mouseInput resRef)
        setCursorPosCallback win $ Just (motion resRef)
        mainLoop resRef win
        terminate

mainLoop :: IORef Resources -> Window -> IO ()
mainLoop _res win = do
    shouldClose <- windowShouldClose win
    unless shouldClose $ do
        waitEvents
        --display _res win
        swapBuffers win
        mainLoop _res win

errorCallback :: ErrorCallback
errorCallback _ = putStrLn

display :: IORef Resources -> WindowRefreshCallback
display _res _ = do
    r <- readIORef _res
    let vertexCount = 2 * lineCount r
        lineBuf     = lineBuffer r
        quadBuf     = quadBuffer r
        panelBuf    = panelBuffer r
        shader      = shaders r
        pbi         = panelBlockIndex shader
        pos         = vertexPosition shader
        prog        = program shader
    putStrLn "glClearColor"
    glClearColor 0.0 0.0 0.3 1.0
    printGLErrors
    putStrLn "glClear"
    glClear GL_COLOR_BUFFER_BIT
    printGLErrors
    putStrLn "glUseProgram"
    glUseProgram prog
    printGLErrors
    putStrLn "glVertexAttribPointer"
    glVertexAttribPointer pos 2 GL_FLOAT GL_FALSE 0 nullPtr
    printGLErrors
    putStrLn "glEnableVertexAttribArray"
    glEnableVertexAttribArray pos
    printGLErrors
    putStrLn "glBindBufferBase"
    glBindBufferBase GL_UNIFORM_BUFFER pbi panelBuf
    printGLErrors
    putStrLn "glBindBuffer"
    glBindBuffer GL_ARRAY_BUFFER quadBuf
    printGLErrors
    putStrLn "glDrawArrays (GL_TRIANGLE_STRIP)"
    glDrawArrays GL_TRIANGLE_STRIP 0 4
    printGLErrors
    putStrLn "glUseProgram 0"
    glUseProgram 0
    printGLErrors
    putStrLn "glBindBuffer"
    glBindBuffer GL_ARRAY_BUFFER lineBuf
    printGLErrors
    putStrLn "glDrawArrays (GL_LINES)"
    glDrawArrays GL_LINES 0 vertexCount
    printGLErrors
    putStrLn "glBindBuffer 0"
    glBindBuffer GL_ARRAY_BUFFER 0
    printGLErrors

serializePanels :: [Panel] -> [Float]
serializePanels = foldr foldingFunction []
    where
    foldingFunction (Line (V2 x1 y1) (V2 x2 y2), strength) acc =
        x1:y1:x2:y2:strength:acc

reshape :: IORef Resources -> WindowSizeCallback
reshape _res _ width height = do
    modifyIORef _res (\x -> x { winSize = (width,height) })
    updateFlow _res
    r <- readIORef _res
    let qbuf = quadBuffer r
        shader = shaders r
        prog = program shader
        tu  = transformUniform shader
        mvp  = serializeMatrix4 $ getOrtho 0 (fromIntegral width)
                                           0 (fromIntegral height)
    putStrLn "glViewport"
    glViewport 0 0 (toEnum width) (toEnum height)
    printGLErrors
    putStrLn "glUseProgram"
    glUseProgram prog
    printGLErrors
    putStrLn "glUniformMatrix4fv"
    withArray mvp $ \ptr -> glUniformMatrix4fv tu 1 GL_FALSE ptr
    printGLErrors
    let quad = [               0.0,                 0.0
              , fromIntegral width,                 0.0
              ,                0.0, fromIntegral height
              , fromIntegral width, fromIntegral height] :: [GLfloat]
    withArrayLen quad $ \len ptr->
        do
        let bytes = len * sizeOf (head quad)
        putStrLn "glNamedBufferData"
        glNamedBufferData qbuf (toEnum bytes) ptr GL_STATIC_DRAW
        printGLErrors

motion :: IORef Resources -> CursorPosCallback
motion _res _ x y = modifyIORef _res (\r -> r {mousePos = V2 (double2Float x) (double2Float y)})

mouseInput :: IORef Resources -> MouseButtonCallback
mouseInput _res _ _button MouseButtonState'Pressed _ =
    case _button of
        MouseButton'1 -> click _res
        _             -> return ()
mouseInput _ _ _ _ _ = return ()

--translateGLUTCoords :: Size -> Position -> FV2
--translateGLUTCoords (Size _ height) (Position x y) = V2 (fromIntegral x) (fromIntegral $ height - y)

findEndpoint :: FV2 -> [Line] -> Maybe FV2
findEndpoint p = find (\v -> qd v p < 100) . foldr (\(Line v1 v2) acc-> v1:v2:acc) []

findClosedPolygon :: [Line] -> ([Line], [Line])
findClosedPolygon ls = ([],ls) --(connected,rest)
    where
    (connected,rest) = partition (\(Line v1 v2)->
        elem v1 intersectionPoints && elem v2 intersectionPoints) ls
    intersectionPoints = foldl' (\acc (Line _ v1)->
        case find (\(Line v2 _) -> v1 == v2) ls of
            Nothing -> acc
            Just _  -> v1:acc) [] ls

click :: IORef Resources -> IO ()
click _res = do
    r <- readIORef _res
    let ls       = lineList r
        lCount   = lineCount r
        position = mousePos r
        partial  = partialLine r
        polys    = polygons r
        existing = findEndpoint position ls
        p        = head $ catMaybes (existing : [Just position])
    case partial of
        Nothing ->
            modifyIORef _res (\x -> x { partialLine = Just p } )
        Just oldPoint ->
            unless (oldPoint == p) $ do
                let newLines = Line oldPoint p : ls
                    pair = findClosedPolygon newLines
                case pair of
                    ([],_)         ->
                        modifyIORef _res (\x ->
                            x { lineList = newLines
                              , lineCount = lCount + 1
                              , partialLine = Nothing } )
                    (polygon,rest) ->
                        modifyIORef _res (\x ->
                            x { lineList = rest
                              , lineCount = toEnum $ length rest
                              , polygons = polygon:polys
                              , partialLine = Nothing} )
                updateFlow _res

updateFlow :: IORef Resources -> IO ()
updateFlow _res = do
    r <- readIORef _res
    let ls       = lineList r
        panelBuf = panelBuffer r
        shader   = shaders r
        prog     = program shader
        npu      = numPanelsUniform shader
        newFlow  = calculateFlow ls
        newPanelCount = toEnum $ length newFlow
    print newFlow
    print newPanelCount
    glUseProgram prog
    glUniform1i npu newPanelCount
    let mydata = serializePanels newFlow
    withArrayLen (serializePanels newFlow :: [GLfloat]) $ \len ptr->
        do
        let bytes = len * sizeOf (head mydata)
        print len
        print bytes
        glNamedBufferData panelBuf (toEnum bytes) ptr GL_STATIC_DRAW
    modifyIORef _res (\x -> x { panels = newFlow
                      , panelCount = newPanelCount} )
    where
    --serializeLines = concatMap serializeVector2 . concatMap deLine
    deLine (Line v1 v2) = [v1,v2]


calculateFlow :: [Line] -> [Panel]
calculateFlow ls =
    let numberedList = zip [(0::Int)..] ls
        equations = map (\z@(_,target) ->
            (foldr (foldingFunction z) [] numberedList, -freestreamStrength target)
            ) numberedList
        leftHandSide = map (map float2Double . fst) equations
        rightHandSide = map (float2Double . snd) equations
        n = length rightHandSide
        strengths = linearSolve (fromLists leftHandSide :: Matrix R)
                                ((n >< 1) rightHandSide) in
    case strengths of
        Nothing -> []
        Just s  -> zip ls (map double2Float . concat $ toLists s)
    where
    foldingFunction (i,controlPoint) (j,panel) acc
        | i == j    = 0.5 : acc
        | otherwise = dot n inducedVelocity / (2*pi) : acc
        where
        inducedVelocity = integralCoefficient (getMidpoint controlPoint) panel
        n = getNormal controlPoint

getOrtho :: Float -> Float -> Float -> Float -> M44 GLfloat
getOrtho left right bottom top =
    V4  (V4   a 0.0 0.0  tx)
        (V4 0.0   b 0.0  ty)
        (V4 0.0 0.0   c  tz)
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

visualizeStream :: (Float, Float, Float, Float, Float) -> [(Line,Float)] -> [Line]
visualizeStream (left, right, bottom, top, delta) sources = zipWith Line coords (zipWith (+) coords normalizedField)
    where
    scalingFactor = 0.9 * delta/norm freestreamVelocity
    normalizedField = parMap rdeepseq ((scalingFactor *^) . totalVelocity sources) coords
    coords = [V2 x y | x <- [left,left+delta..right], y <- [bottom,bottom+delta..top]]

freestreamVelocity :: FV2
freestreamVelocity = V2 25.0 0.0

freestreamStrength :: Line -> Float
freestreamStrength l = getNormal l `dot` freestreamVelocity

integralCoefficient :: FV2 -> Line -> FV2
integralCoefficient tarreadIORef panel = foldl' integration (V2 0 0) chunks
    where
    integration acc v = acc + (tarreadIORef - v) ^* (ds / qd tarreadIORef v)
    (chunks,ds) = discretize panel subDivisions
    subDivisions = 100

totalVelocity :: [(Line,Float)] -> FV2 -> FV2
totalVelocity sources p2 = sourceContrib + freestreamVelocity
    where
    sourceContrib = foldl' (\acc panel-> acc + velocityContribAt p2 panel) (V2 0 0) sources

velocityContribAt :: FV2 -> (Line,Float) -> FV2
velocityContribAt tarreadIORef (panel,strength) = integralCoefficient tarreadIORef panel ^* (strength/(2*pi))
