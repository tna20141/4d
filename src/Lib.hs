module Lib
    ( euclidianDistance
    , Point
    , Vector
    , makePoint
    , eye1
    , getEyeCenter
    , getLineFromPoints
    , projection
    ) where

import qualified Numeric.LinearAlgebra.HMatrix as HM

type Point = HM.Vector Double
type Vector = HM.Vector Double
data Line = Line { lineCoefficientMatrix :: HM.Matrix Double, lineTerms :: HM.Vector Double } deriving Show

makePoint :: Double -> Double -> Double -> Double -> Point
makePoint x y z t = HM.vector [x, y, z, t]

makeVector :: Double -> Double -> Double -> Double -> Vector
makeVector = makePoint

data EyeConfig = EyeConfig { retinaRadius :: Double, convergenceDistance :: Double }

data EyePosition = EyePosition { convergencePoint :: Point, direction :: Vector }

data Eye = Eye { config :: EyeConfig, position :: EyePosition }

eye1 :: Eye
eye1 = Eye {
    config = EyeConfig { retinaRadius = 10, convergenceDistance = 30 },
    position = EyePosition { convergencePoint = makePoint 1 1 1 1, direction = makeVector (-1) (-1) (-1) (-1) }
    }

euclidianDistance :: Point -> Point -> Double
euclidianDistance a b = sqrt . sum . map (**2) $ zipWith (-) (HM.toList a) (HM.toList b)

euclidianLength :: Vector -> Double
euclidianLength = euclidianDistance (makePoint 0 0 0 0)

getEyeCenter :: Eye -> Point
getEyeCenter Eye { config = eyeConfig, position = eyePosition } =
  let
    ratio = (/ (euclidianLength . direction $ eyePosition)) (convergenceDistance eyeConfig)
  in
    (+ convergencePoint eyePosition) . (* direction eyePosition) . HM.scalar $ ratio

getLineFromPoints :: Point -> Point -> Line
getLineFromPoints a b =
  let
    l = a - b
    a' = HM.toList a
    l' = HM.toList l
    coefficientXy = [l'!!1, -(head l'), 0, 0]
    coefficientYz = [0, l'!!2, -(l'!!1), 0]
    coefficientZt = [0, 0, l'!!3, -(l'!!2)]
    termXy = HM.vector coefficientXy HM.<.> a
    termYz = HM.vector coefficientYz HM.<.> a
    termZt = HM.vector coefficientZt HM.<.> a
  in
    Line {
        lineCoefficientMatrix = HM.matrix 4 (coefficientXy ++ coefficientYz ++ coefficientZt),
        lineTerms = HM.vector [termXy, termYz, termZt]
    }

projection :: Eye -> Point -> Maybe Point
projection eye point =
  let
    Eye { config = eyeConfig, position = eyePosition } = eye
    eyeCenter = getEyeCenter eye
    Line { lineCoefficientMatrix = coefficients, lineTerms = terms } =
        getLineFromPoints point (convergencePoint eyePosition)
    eyeDirection = direction eyePosition
    intersectionCoefficients = HM.fromRows . (++ [eyeDirection]) . HM.toRows $ coefficients
    intersectionTerms = HM.matrix 1 . (++ [eyeDirection HM.<.> eyeCenter]) . HM.toList $ terms
  in
    HM.flatten <$> HM.linearSolve intersectionCoefficients intersectionTerms
