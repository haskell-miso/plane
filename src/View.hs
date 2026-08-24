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
mainView :: context -> props -> Model -> View context Model Action
mainView _ _ m =
  div_
  [ class_ "stage" ]
  [ div_ [ class_ "game", onClick Touched ] content
  , p_ [ class_ "help" ]
    [ text "tap / click / space to fly" ]
  ]
  where
    content =
      [ backgroundView m
      , playerView m
      , pillarsView m
      , messageView m
      , scoreView m
      ]
-----------------------------------------------------------------------------
backgroundView :: Model -> View context Model action
backgroundView Model{..} = wrapper
  [ image gameWidth gameHeight (negate backgroundX) 0 "images/background.png"
  , image gameWidth gameHeight ((fromIntegral gameWidth) - backgroundX) 0 "images/background.png"
  ]
-----------------------------------------------------------------------------
playerView :: Model -> View context Model action
playerView Model{..} = image planeWidth planeHeight playerX y "images/plane.gif"
-----------------------------------------------------------------------------
pillarsView :: Model -> View context Model action
pillarsView Model{..} = wrapper $ fmap pillarView pillars
-----------------------------------------------------------------------------
pillarView :: Pillar -> View context Model action
pillarView Pillar{..} =
  let imageName = if pillarKind == Top then "images/topRock.png" else "images/bottomRock.png"
  in image pillarWidth pillarHeight pillarX pillarY imageName
-----------------------------------------------------------------------------
messageView :: Model -> View context Model action
messageView Model{..} = case state of
  GameOver -> image 250 45 115 150 "images/textGameOver.png"
  Start    -> image 250 45 115 150 "images/textGetReady.png"
  _        -> emptyView
-----------------------------------------------------------------------------
scoreView :: Model -> View context Model action
scoreView Model{..} = p_ [ class_ "score" ] [ text (ms score) ]
-----------------------------------------------------------------------------
wrapper :: [View context Model action] -> View context Model action
wrapper = div_ []
-----------------------------------------------------------------------------
emptyView :: View context Model action
emptyView = wrapper []
-----------------------------------------------------------------------------
image :: Int -> Int -> Double -> Double -> MisoString -> View context Model action
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
