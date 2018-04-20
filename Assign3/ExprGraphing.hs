module ExprGraphing where

import Graphics.Rendering.Chart.Easy hiding (Const)
import Graphics.Rendering.Chart.Backend.Diagrams
import ExprType
import ExprPretty
import ExprDiff
import ExprParser
import qualified Data.Map.Strict as Map

graph :: String -> Double -> Double -> (Expr Double) -> IO ()
graph v a b expr = toFile def "graph.html" $ do
    layout_title .= "Graph"
    setColors [opaque red]
    plot (line (show expr) [signal v (init [a, (a+0.1)..b] ++ [b]) expr])

signal :: String -> [Double] -> (Expr Double) -> [(Double,Double)]
signal v xs expr = [ (x, eval (Map.fromList [(v, x)]) expr) | x <- xs ]
