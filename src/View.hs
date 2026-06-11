-----------------------------------------------------------------------------
module View where
-----------------------------------------------------------------------------
import           Miso
import qualified Miso.CSS    as CSS
import           Miso.Html
import           Miso.Html.Property
-----------------------------------------------------------------------------
import           Constants
import           Model
-----------------------------------------------------------------------------
mainView :: props -> Model -> View Model Action
mainView _ m = wrapper [ div_ [ CSS.style_ style', onClick Touched ] content ]
  where
    style' =
      [ ("width", (ms gameWidth) <> "px")
      , ("height", (ms gameHeight) <> "px")
      , ("overflow", "hidden")
      , ("position", "absolute")
      , ("top", "50%")
      , ("left", "50%")
      , ("transform", "translateX(-240px) translateY(-240px)")
      ]
    content =
      [ backgroundView m
      , playerView m
      , pillarsView m
      , messageView m
      , scoreView m
      ]
-----------------------------------------------------------------------------
backgroundView :: Model -> View Model action
backgroundView Model{..} = wrapper
  [ image gameWidth gameHeight (negate backgroundX) 0 "images/background.png"
  , image gameWidth gameHeight ((fromIntegral gameWidth) - backgroundX) 0 "images/background.png"
  ]
-----------------------------------------------------------------------------
playerView :: Model -> View Model action
playerView Model{..} = image planeWidth planeHeight playerX y "images/plane.gif"
-----------------------------------------------------------------------------
pillarsView :: Model -> View Model action
pillarsView m@Model{..} = wrapper $ fmap (pillarView m) pillars
-----------------------------------------------------------------------------
pillarView :: Model -> Pillar -> View Model action
pillarView Model{} Pillar{..} =
  let imageName = if pillarKind == Top then "images/topRock.png" else "images/bottomRock.png"
  in image pillarWidth pillarHeight pillarX pillarY imageName
-----------------------------------------------------------------------------
messageView :: Model -> View Model action
messageView Model{..} = case state of
  GameOver -> image 250 45 115 150 "images/textGameOver.png"
  Start    -> image 250 45 115 150 "images/textGetReady.png"
  _        -> emptyView
-----------------------------------------------------------------------------
scoreView :: Model -> View Model action
scoreView Model{..} = p_ [ CSS.style_ style' ] [ text (ms score) ]
  where
    style' =
      [ ("display", "block")
      , ("height", "50px")
      , ("text-align", "center")
      , ("width", "100%")
      , ("position", "absolute")
      , ("y", "70")
      , ("color", "#32a032")
      , ("font-size", "50px")
      , ("font-weight", "bold")
      , ("font-family", "Helvetica, Arial, sans-serif")
      , ("text-shadow", "-1px 0 #005000, 0 1px #005000, 1px 0 #005000, 0 -1px #005000")
      ]
-----------------------------------------------------------------------------
wrapper :: [View Model action] -> View Model action
wrapper = div_ []
-----------------------------------------------------------------------------
emptyView :: View Model action
emptyView = wrapper []
-----------------------------------------------------------------------------
image :: Int -> Int -> Double -> Double -> MisoString -> View Model action
image width height offsetX offsetY file = img_
  [ src_ file
  , CSS.style_
    [ ("display", "block")
    , ("width", (ms width) <> "px")
    , ("height", (ms height) <> "px")
    , ("position", "absolute")
    , ("transform", ms $ "matrix(1,0,0,1," ++ show offsetX ++ ", " ++ show offsetY ++ ")")
    ]
  ]
-----------------------------------------------------------------------------
