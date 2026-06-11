-----------------------------------------------------------------------------
{-# LANGUAGE LambdaCase #-}
-----------------------------------------------------------------------------
module Update where
-----------------------------------------------------------------------------
import           Control.Monad.State hiding ( state )
import           Data.Function
import qualified Data.IntSet as S
import           Data.Maybe (mapMaybe)
-----------------------------------------------------------------------------
import           System.Random
import           Miso
-----------------------------------------------------------------------------
import           Constants
import           Model
-----------------------------------------------------------------------------
updateModel :: Action -> Effect parent props Model Action
updateModel = \case
  Time newTime ->
    get >>= step newTime
  Keyboard keys
    | S.member 32 keys -> modify jump
    | otherwise -> pure ()
  Touched ->
    modify jump
  NewPillars height ->
    modify $ \m -> m { pillars = generatePillars height <> pillars m }
-----------------------------------------------------------------------------
jump :: Model -> Model
jump m = m & transitionState & updatePlayerVelocity
-----------------------------------------------------------------------------
step :: Double -> Model -> Effect parent props Model Action
step newTime m = do
    put newModel
    batch (if shouldAddPillar then [ pillarsTransition ] else [ ])
  where
    pillarsTransition = NewPillars <$> randomRIO (minPillarHeight, gameHeight - minPillarHeight - round gapHeight)
    shouldAddPillar = timeToPillar newModel == timeBetweenPillars && state newModel == Play
    newModel = m & updateTime newTime
                 & updatePlayerY
                 & updateBackground
                 & applyPhysics
                 & updatePillars
                 & checkFailState
                 & updateScore
-----------------------------------------------------------------------------
updatePillars :: Model -> Model
updatePillars m@Model{..} = m { timeToPillar = newTimeToPillar, pillars = updatedPillars }
  where
    newTimeToPillar =
      if timeToPillar <= 0 then timeBetweenPillars
      else if state == Play then timeToPillar - delta
      else timeToPillar
    updatedPillars = mapMaybe scroll pillars
    scroll p =
      let newX = pillarX p - foregroundScrollV * delta
      in if newX > negate (fromIntegral pillarWidth)
         then Just (p { pillarX = newX })
         else Nothing
-----------------------------------------------------------------------------
updateTime :: Double -> Model -> Model
updateTime newTime m@Model{..} = m { time = newTime, delta = newTime - time }
-----------------------------------------------------------------------------
updatePlayerY :: Model -> Model
updatePlayerY m@Model{..} = m { y = newY }
  where
    newY =
      if state == Start then y + (sin (backgroundX / 10))
      else if state == Play || state == GameOver && not (playerOffScreen m) then y + vy * delta
      else y
-----------------------------------------------------------------------------
isColliding :: Model -> Pillar -> Bool
isColliding Model{..} p =
       playerLeft < pillarRight
    && playerRight > pillarLeft
    && playerTop < pillarBottom
    && playerBottom > pillarTop
  where
    playerLeft = playerX + epsilon
    playerTop = y
    playerRight = playerX + fromIntegral planeWidth - epsilon
    playerBottom = y + fromIntegral planeHeight
    pillarLeft = (pillarX p) + epsilon
    pillarTop = (pillarY p)
    pillarRight = (pillarX p) + fromIntegral pillarWidth - epsilon
    pillarBottom = (pillarY p) + fromIntegral (pillarHeight p)
-----------------------------------------------------------------------------
checkFailState :: Model -> Model
checkFailState m@Model{..} = m { state = newState }
  where
    newState = if state == Play && (playerOffScreen m || playerCollidedWithPillar) then GameOver else state
    playerCollidedWithPillar = any (isColliding m) pillars
-----------------------------------------------------------------------------
updateBackground :: Model -> Model
updateBackground m@Model{..} = m { backgroundX = newBackgroundX }
  where
    newBackgroundX =
      if backgroundX > (fromIntegral gameWidth) then 0
      else if state == GameOver then backgroundX
      else backgroundX + (delta * backgroundScrollV)
-----------------------------------------------------------------------------
applyPhysics :: Model -> Model
applyPhysics m@Model{..} = m { vy = newVy }
  where
    newVy = if state == Play || state == GameOver && not (playerOffScreen m) then vy + delta * gravity else 0
-----------------------------------------------------------------------------
generatePillars :: Int -> [Pillar]
generatePillars bottomHeight =
  [ Pillar
    { pillarX = x
    , pillarY = fromIntegral topHeight + gapHeight
    , pillarHeight = bottomHeight
    , pillarKind = Bottom
    , pillarPassed = False
    }
  , Pillar
    { pillarX = x
    , pillarY = 0
    , pillarHeight = topHeight
    , pillarKind = Top
    , pillarPassed = False
    }
  ]
  where
    x = fromIntegral gameWidth
    topHeight = gameHeight - bottomHeight - round gapHeight
-----------------------------------------------------------------------------
updateScore :: Model -> Model
updateScore m@Model{..} = m { pillars = newPillars, score = newScore }
  where
    (anyPassed, newPillars) = foldr tally (False, []) pillars
    tally p (acc, ps)
      | not (pillarPassed p) && pillarX p < playerX = (True, p { pillarPassed = True } : ps)
      | otherwise = (acc, p : ps)
    newScore = if anyPassed then score + 1 else score
-----------------------------------------------------------------------------
transitionState :: Model -> Model
transitionState m@Model{..} =
  if state == GameOver && playerOffScreen m
  then initialModel
  else m { state = if state == Start then Play else state }
-----------------------------------------------------------------------------
updatePlayerVelocity :: Model -> Model
updatePlayerVelocity m@Model{..} = m { vy = if state == Play then jumpSpeed else vy }
-----------------------------------------------------------------------------
playerOffScreen :: Model -> Bool
playerOffScreen Model{..} = y < 0 || y > fromIntegral gameHeight
-----------------------------------------------------------------------------
