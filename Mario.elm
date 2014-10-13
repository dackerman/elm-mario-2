import Keyboard
import Window

-- MODEL
type Block =
        { x : Float
        , y : Float }

type GameState =
        { x : Float
        , y : Float
        , vx : Float
        , vy : Float
        , dir : String
        , blocks : [Block]}

renderBlock : Float -> Block -> Form
renderBlock h {x,y} = toForm (image 35 35 "/imgs/block.png") |> move (x, y - h / 2 )

mario : GameState
mario = { x=0, y=0, vx=0, vy=0, dir="right", blocks=initialBlocks }

initialBlocks : [Block]
initialBlocks = let genblock i = {x=50 * i, y=100}
                in map genblock [0..10] 

type Arrows = { x : Int, y : Int }

-- UPDATE -- ("m" is for Mario)
jump : Arrows -> GameState -> GameState 
jump {y} m = if y > 0 && m.y == 0 then
                { m | vy <- 5 } else 
                m

gravity : Time -> GameState -> GameState
gravity t m = if m.y > 0 then 
                 { m | vy <- m.vy - t/4 } else 
                 m

physics : Time -> GameState -> GameState
physics t m = { m 
            | x <- m.x + t*m.vx 
            , y <- max 0 (m.y + t*m.vy) }

walk : Arrows -> GameState -> GameState
walk {x} m = { m | vx <- toFloat x
                 , dir <- if | x < 0     -> "left"
                             | x > 0     -> "right"
                             | otherwise -> m.dir }

step : (Time, Arrows) -> GameState -> GameState
step (dt, keys) =
  jump keys >> gravity dt >> walk keys >> physics dt


-- DISPLAY
render : (Int,Int) -> GameState -> Element
render (w',h') mario =
  let (w,h) = (toFloat w', toFloat h')
      verb = if | mario.y  >  0 -> "jump"
                | mario.vx /= 0 -> "walk"
                | otherwise     -> "stand"
      src = "/imgs/mario/" ++ verb ++ "/" ++ mario.dir ++ ".gif"
  in collage w' h'
     ([ rect w h  |> filled (rgb 174 238 238)
      , rect w 50 |> filled (rgb 74 163 41)
                  |> move (0, 24 - h/2)
      , toForm (image 35 35 src) |> move (mario.x, mario.y + 62 - h/2)
      ] ++ (map (renderBlock h) mario.blocks))

-- MARIO
input : Signal (Time, Arrows)
input = let delta = lift (\t -> t/20) (fps 25)
        in  sampleOn delta (lift2 (,) delta Keyboard.arrows)

main : Signal Element
main = lift2 render Window.dimensions (foldp step mario input)

