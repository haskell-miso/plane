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
gameContainerStyle :: [(MisoString, MisoString)]
gameContainerStyle =
  [ ("width", (ms gameWidth) <> "px")
  , ("height", (ms gameHeight) <> "px")
  , ("overflow", "hidden")
  , ("position", "absolute")
  , ("top", "50%")
  , ("left", "50%")
  , ("transform", "translateX(-240px) translateY(-240px)")
  ]
-----------------------------------------------------------------------------
mainView :: props -> Model -> View Model Action
mainView _ m = wrapper [ div_ [ CSS.style_ gameContainerStyle, onClick Touched ] content ]
  where
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
pillarsView Model{..} = wrapper $ fmap pillarView pillars
-----------------------------------------------------------------------------
pillarView :: Pillar -> View Model action
pillarView Pillar{..} =
  let imageName = if pillarKind == Top then "images/topRock.png" else "images/bottomRock.png"
  in image pillarWidth pillarHeight pillarX pillarY imageName
-----------------------------------------------------------------------------
messageView :: Model -> View Model action
messageView Model{..} = case state of
  GameOver -> image 250 45 115 150 "images/textGameOver.png"
  Start    -> image 250 45 115 150 "images/textGetReady.png"
  _        -> emptyView
-----------------------------------------------------------------------------
scoreStyle :: [(MisoString, MisoString)]
scoreStyle =
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
scoreView :: Model -> View Model action
scoreView Model{..} = p_ [ CSS.style_ scoreStyle ] [ text (ms score) ]
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
    , ("transform", ms $ "translate3d(" ++ show offsetX ++ "px," ++ show offsetY ++ "px,0)")
    ]
  ]
-----------------------------------------------------------------------------
