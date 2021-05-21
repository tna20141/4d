module Lib
    ( someFunc
    ) where

newtype Point1D = Point1D { x1d :: Double }
data Point2D = Point2D { x2d :: Double, y2d :: Double }
data Point3D = Point3D { x3d :: Double, y3d :: Double, z3d :: Double }
data Point4D = Point4D { x4d :: Double, y4d:: Double, z4d :: Double, t4d :: Double }
type Point = Point4D
type Vector4D = Point4D
type Vector = Vector4D
type Vector3D = Point3D
type Vector2D = Point2D
type Vector1D = Point1D

data EyeConfig = EyeConfig { retinaRadius :: Double, convergenceDistance :: Double }

data EyePosition = EyePosition { convergencePoint :: Point, direction :: Vector }

data Eye = Eye { config :: EyeConfig, position :: EyePosition }

projection :: Eye -> point4D -> Maybe Point3D
projection = undefined



someFunc :: IO ()
someFunc = putStrLn "someFunc"
