import Lib
import Compute
import System.Random
import Control.Monad ( replicateM )

createColor :: Int -> Int -> Int -> Color
createColor r g b = Color{_r=r, _g=g, _b=b}

createPoint :: Int -> Int -> Point
createPoint x y = Point{_x=x, _y=y}

testIn :: [In]
testIn =
    [
        In{_color=createColor 21 21 21, _point=createPoint 21 21},
        In{_color=createColor 31 31 31, _point=createPoint 22 22},
        In{_color=createColor 41 41 41, _point=createPoint 23 23},
        In{_color=createColor 51 51 51, _point=createPoint 24 24},
        In{_color=createColor 61 61 61, _point=createPoint 25 25},
        In{_color=createColor 71 71 71, _point=createPoint 26 26}
    ]

main :: IO ()
main = do
    return ()
