module AutomatShow(viewNKA,viewDKA) where

import Graphics.Gloss
import Types
import Data.List(intercalate)

------------------------------------------------
-- параметры
------------------------------------------------

radius :: Float
radius = 300

nodeR :: Float
nodeR = 45

labelLiftBase :: Float
labelLiftBase = 85

curveSteps :: Int
curveSteps = 50

------------------------------------------------
-- отображение
------------------------------------------------

viewNKA :: NKA -> IO ()
viewNKA nka =
  display (InWindow "NKA" (1400,1000) (100,100))
          white
          (drawNKA nka)

viewDKA :: DKA -> IO ()
viewDKA dka =
  display (InWindow "DKA" (1400,1000) (100,100))
          (makeColorI 248 249 252 255)
          (drawDKA dka)

------------------------------------------------
-- layout
------------------------------------------------

layoutCircular :: [State] -> State -> Point
layoutCircular states s =
 case lookup s coords of
   Just p -> p
   Nothing -> (0,0)
 where
   n = max 1 (length states)
   angles = [2*pi*fromIntegral i / fromIntegral n | i <- [0..n-1]]

   coords =
     zip states
       [ (radius*cos a, radius*sin a)
       | a <- angles ]

------------------------------------------------
-- labels
------------------------------------------------

labels :: [Char] -> String
labels = intercalate "," . map (:[])

------------------------------------------------
-- Bézier
------------------------------------------------

bezier3 :: Point -> Point -> Point -> Int -> [Point]
bezier3 p0 p1 p2 steps =
 let lerp (x1,y1) (x2,y2) t =
       (x1 + (x2-x1)*t, y1 + (y2-y1)*t)

     go i
       | i > steps = []
       | otherwise =
           let t = fromIntegral i / fromIntegral steps
               a = lerp p0 p1 t
               b = lerp p1 p2 t
           in lerp a b t : go (i+1)

 in go 0

midPoint :: [Point] -> Point
midPoint pts = pts !! (length pts `div` 2)

------------------------------------------------
-- curve (Graphviz-style stable)
------------------------------------------------
labelOffset :: Point -> Point -> Float -> Point
labelOffset (cx,cy) (lx,ly) k =
 let dx = lx - cx
     dy = ly - cy
     len = sqrt (dx*dx + dy*dy)
     nx = -dy / max 1 len
     ny =  dx / max 1 len
 in (lx + nx * k, ly + ny * k)


clampToCircle :: Point -> Point -> Point -> Point
clampToCircle (x1,y1) (x2,y2) p =
 let dx = x2 - x1
     dy = y2 - y1
     len = sqrt (dx*dx + dy*dy)

     ux = dx / max 1 len
     uy = dy / max 1 len

 in ( fst p + ux * nodeR
    , snd p + uy * nodeR )


curve :: Point -> Point -> Float -> Float -> ([Point], Point)
curve (x1,y1) (x2,y2) lift sign =
 let
     dx = x2 - x1
     dy = y2 - y1
     len = sqrt (dx*dx + dy*dy)

     nx = -dy / max 1 len
     ny =  dx / max 1 len

     mx = (x1 + x2) / 2
     my = (y1 + y2) / 2

     ux = dx / max 1 len
     uy = dy / max 1 len

     edgePadding = nodeR + 4

     start = (x1 + ux * edgePadding, y1 + uy * edgePadding)
     endp  = (x2 - ux * edgePadding, y2 - uy * edgePadding)

     -- 🔥 ВАЖНО: выталкиваем контрольную точку дальше наружу
     cx = mx + nx * (lift + nodeR) * sign
     cy = my + ny * (lift + nodeR) * sign

     pts = bezier3 start (cx,cy) endp curveSteps

 in (pts,(cx,cy))

------------------------------------------------
-- self-loop
------------------------------------------------

selfLoop :: Point -> String -> Picture
selfLoop (x,y) lab =
 let r = nodeR + 25

     points =
       [ (x + nodeR, y)
       , (x + r, y + r)
       , (x, y + r + 30)
       , (x - r, y + r)
       , (x - nodeR, y)
       ]

     arrow =
       Polygon
         [ (x - nodeR, y + 5)
         , (x - nodeR - 10, y)
         , (x - nodeR, y - 5)
         ]

 in Pictures
  [ Color black $ Line points
  , Color black arrow
  , Translate (x - 10) (y + r + 35) $
      Scale 0.18 0.18 $
      Text lab
  ]

------------------------------------------------
-- states
------------------------------------------------

drawState finals layout s =
 let (x,y) = layout s
     name = unState s
     len  = length name

     fill
       | name == "∅" = greyN 0.85
       | s `elem` finals  = makeColorI 193 230 190 255
       | otherwise        = makeColorI 187 222 251 255

     -- 🔥 адаптивный масштаб текста
     sc = max 0.12 (0.25 - fromIntegral (len - 1) * 0.03)

     -- 🔥 более точное центрирование по длине строки
     tx = x - 6 * fromIntegral len
     ty = y - 10

 in Pictures
  [ Translate x y $
      Color fill $
      circleSolid nodeR

  , Translate x y $
      Color black $
      circle nodeR

  , if s `elem` finals
      then Translate x y $
           Color black $
           circle (nodeR - 8)
      else Blank

  -- 🔥 текст теперь адаптивный и не вылезает за круг
  , Translate tx ty $
      Scale sc sc $
      Color black $
      Text name
  ]

------------------------------------------------
-- start arrow
------------------------------------------------

drawStart st layout =
 let (x,y) = layout st

     start = (x + 140, y)
     end   = (x + nodeR, y)

     arrow =
       Polygon
         [ (fst end, snd end)
         , (fst end + 10, snd end - 5)
         , (fst end + 10, snd end + 5)
         ]

 in Pictures
  [ Color red $ Line [start,end]
  , Color red arrow
  ]


arrowHead :: [Point] -> Picture
arrowHead pts =
 let (x1,y1) = last (init pts)
     (x2,y2) = last pts

     dx = x2 - x1
     dy = y2 - y1
     l  = sqrt (dx*dx + dy*dy)

     ux = dx / max 1 l
     uy = dy / max 1 l

 in Color black $
    Polygon
      [ (x2,y2)
      , (x2 - 18*ux + 7*uy, y2 - 18*uy - 7*ux)
      , (x2 - 18*ux - 7*uy, y2 - 18*uy + 7*ux)
      ]
------------------------------------------------
-- avoidance (no overlap with states)
------------------------------------------------

avoidStates :: Point -> [State] -> (State -> Point) -> Point
avoidStates (x,y) states layout =
 foldr push (x,y) states
 where
   push s acc =
     let (sx,sy) = layout s
         dx = x - sx
         dy = y - sy
         d  = sqrt (dx*dx + dy*dy)
         minD = nodeR + 20
     in if d < minD && d > 0
        then (sx + dx/d * minD, sy + dy/d * minD)
        else acc

------------------------------------------------
-- transitions DKA (stable Graphviz-like)
------------------------------------------------

drawTransitionsDKA trans layout =
 Pictures (map draw grouped)
 where

  grouped = foldr collect [] trans

  collect ((f,a),t) acc =
    case lookup (f,t) acc of
      Just xs ->
        ((f,t),a:xs)
         : filter ((/= (f,t)) . fst) acc
      Nothing ->
        ((f,t),[a]) : acc

  stateList = map (\((s,_),_) -> s) trans

  hasReverse a b =
    any (\((x,_),y) -> x==b && y==a) trans

  draw ((f,t),labs)
    | f == t =
        selfLoop (layout f) (labels labs)

    | otherwise =
      let p1 = layout f
          p2 = layout t

          sign = if hasReverse f t then 1 else -1
          lift = labelLiftBase + fromIntegral (length labs) * 10

          (pts,(cx,cy)) = curve p1 p2 lift sign

          (lx,ly) = midPoint pts

          -- 👇 главное улучшение
          (lx',ly') = labelOffset (cx,cy) (avoidStates (lx,ly) stateList layout) 18

      in Pictures
        [ Color black $ Line pts
        , arrowHead pts
        , Translate lx' ly' $
            Scale 0.18 0.18 $
            Color black $
            Text (labels labs)
        ]

------------------------------------------------
-- NKA transitions
------------------------------------------------

showTerm (Term c) = [c]
showTerm Eps = "ε"

drawTransitionsNKA trans layout =
 Pictures (map draw grouped)
 where

  grouped = foldr collect [] trans

  collect (f,tm,t) acc =
    case lookup (f,t) acc of
      Just xs ->
        ((f,t),tm:xs)
         : filter ((/= (f,t)) . fst) acc
      Nothing ->
        ((f,t),[tm]) : acc

  stateList = map (\(s,_,_) -> s) trans

  draw ((f,t),labs)
    | f == t =
        selfLoop (layout f) (concatMap showTerm labs)

    | otherwise =
      let p1 = layout f
          p2 = layout t

          (pts,(cx,cy)) = curve p1 p2 90 1

          (lx,ly) = midPoint pts

          -- 👇 улучшение такое же
          (lx',ly') = labelOffset (cx,cy) (avoidStates (lx,ly) stateList layout) 18

      in Pictures
        [ Color black $ Line pts
        , arrowHead pts
        , Translate lx' ly' $
            Scale 0.18 0.18 $
            Color black $
            Text (concatMap showTerm labs)
        ]

------------------------------------------------
-- full render
------------------------------------------------

drawDKA dka =
 let states = dkaStates dka
     layout = layoutCircular states
 in Pictures
  [ Pictures [drawState (dkaFinal dka) layout s | s<-states]
  , drawTransitionsDKA (dkaTransitions dka) layout
  , drawStart (dkaStart dka) layout
  ]

drawNKA nka =
 let states = nkaStates nka
     layout = layoutCircular states
 in Pictures
  [ Pictures [drawState (nkaFinal nka) layout s | s<-states]
  , drawTransitionsNKA (nkaTransitions nka) layout
  , drawStart (nkaStart nka) layout
  ]