module Math exposing (..)

type MetalType = Cu | Al

-- даны фи и киловатты найти амперы
type VType = OnePhase | ThreePhase

wt2Amp : Float -> Float -> VType -> Float
wt2Amp phi kwt vtype =
    let
      k = if vtype == OnePhase then 1 else sqrt 3
    in
      kwt / ( k * 220 * phi)


amp2Wt : Float -> Float -> VType -> Float
amp2Wt phi amp vtype =
    let
      k = if vtype == OnePhase then 1 else sqrt 3
    in
      k * amp * 220 * phi
      
deltaU : Float -> Int -> MetalType -> VType -> Float -> Float ->  Maybe (Float, Float) -> Float
deltaU amp l mtype vtype crossSection cosf diameters =
  let
    k = if vtype == OnePhase then 2 else sqrt 3
    g = if mtype == Cu then 53 else 31.7
    r = 1000 / ( g * crossSection )
    ( betweenLines, diameter ) =
    case diameters of
      Just a  -> a
      Nothing -> (1, 1)

    x = 0.1145 * logBase 10 (2 * betweenLines / diameter) + 0.016
  in
    k * amp * ( r * cosf + x * sqrt (1 - cosf*cosf)) * toFloat l
