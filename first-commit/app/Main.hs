{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Main where

import           Control.Concurrent         (forkIO, threadDelay)
import           Control.Lens               (makeLenses, use, (.=), (^.), (&), (.~), (%~), ix, preuse, at, non, (+=), imap, (^..), Traversal', Getting, Fold, Lens', (+~), Const)
import           Control.Lens.Unsound       (lensProduct)
import           Control.Monad.State
import           Data.Map                   (Map, fromList)
import qualified Data.Map                   as M
import           Data.Maybe
import           Data.Set                   (Set)
import qualified Data.Set                   as S
import           Data.Void
import           Linear

import           Brick
import           Brick.BChan
import qualified Brick.Widgets.Border       as B
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Center       as C
import qualified Graphics.Vty               as V
import qualified Graphics.Vty.CrossPlatform as V
import           Data.Text                  (Text)
import           Text.Megaparsec            hiding (State)
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Monoid (First, Endo)
import Data.Semigroup (Max)

------------------------------------------------------------
-- AST

data Command
  = Wait
  | Move
  | TL
  | TR
  | Harvest
  deriving (Eq, Ord, Show)

type Program = [Command]

------------------------------------------------------------
-- Parsing
------------------------------------------------------------

type Parser = Parsec Void Text

--------------------------------------------------
-- Lexer

sc :: Parser ()
sc = L.space
  space1
  (L.skipLineComment "//")
  (L.skipBlockComment "/*" "*/")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

reserved :: Text -> Parser ()
reserved w = (lexeme . try) $ string' w *> notFollowedBy alphaNumChar

--------------------------------------------------
-- Parser

parseCommand :: Parser Command
parseCommand =
      Move    <$ reserved "move"
  <|> TL      <$ reserved "left"
  <|> TR      <$ reserved "right"
  <|> Harvest <$ reserved "harvest"

parseProgram :: Parser Program
parseProgram = many parseCommand

------------------------------------------------------------
-- State machine
------------------------------------------------------------

data Robot = Robot
  { _location     :: V2 Int
  , _direction    :: V2 Int
  , _robotProgram :: Program
  }
  deriving (Eq, Ord, Show)

data Item = Resource Char
  deriving (Eq, Ord, Show)

data GameState = GameState
  { _baseProgram :: Program
  , _robots      :: [Robot]
  , _world       :: [[Char]]
  , _inventory   :: Map Item Int
  }
  deriving (Eq, Ord, Show)

makeLenses ''Robot
makeLenses ''GameState

step :: State GameState ()
step = do
  rs <- use robots
  rs' <- catMaybes <$> forM rs stepRobot
  robots .= rs'

doStep :: GameState -> GameState
doStep = execState step

stepRobot :: Robot -> State GameState (Maybe Robot)
stepRobot r = case r ^. robotProgram of
  []        -> return Nothing
  (cmd : p) -> Just <$> exec cmd (r & robotProgram .~ p)

temp :: Traversal' GameState Char
temp = world . ix 2 . ix 3

temp' :: Fold GameState Char
temp' = temp

temp'' :: Getting (First Char) GameState Char
temp'' = temp

temp''' :: Getting (Endo Char) GameState Char
temp''' = temp

temp'''' :: Getting (Max Char) GameState Char
temp'''' = temp

b :: Lens' GameState (Maybe Int)
b = inventory . at (Resource 'a')

b' :: Traversal' GameState Int
b' = inventory . ix (Resource 'a')

c :: Lens' GameState Int
c = b . non 0

worksToo :: Map [Char] Integer
worksToo = fromList [("hell",1)] & ix "hello" +~ 2

exec :: Command -> Robot -> State GameState Robot
exec Wait    r = return r
exec Move    r = return $ (r & location %~ (^+^ (r ^. direction)))
exec TL      r = return $ (r & direction %~ vLeft)
exec TR      r = return $ (r & direction %~ vRight)
exec Harvest r = do
  let V2 row col = r ^. location
  mh <- preuse $ world . ix row . ix col
  case mh of
    Nothing -> return ()
    Just h  -> do
      world . ix row . ix col .= ' '
      inventory . at (Resource h) . non 0 += 1
  return r

vLeft (V2 x y) = V2 (-y) (x)
vRight (V2 x y) = V2 y (-x)

------------------------------------------------------------
-- UI

data Tick = Tick

type Name = ()

app :: App GameState Tick Name
app = App
  { appDraw         = drawUI
  , appChooseCursor = neverShowCursor
  , appHandleEvent  = handleEvent
  , appStartEvent   = return ()
  , appAttrMap      = const theMap
  }

robotAttr :: AttrName
robotAttr = attrName "robotAttr"

theMap :: AttrMap
theMap = attrMap V.defAttr
  [ (robotAttr, fg V.cyan `V.withStyle` V.bold)
  ]

handleEvent :: BrickEvent Name Tick -> EventM Name GameState ()
handleEvent (AppEvent Tick)                       = modify doStep
handleEvent (VtyEvent (V.EvKey (V.KChar 'q') [])) = halt
handleEvent (VtyEvent (V.EvKey V.KEsc []))        = halt
handleEvent _                                     = return ()

drawUI :: GameState -> [Widget Name]
drawUI g =
  [ C.center $ drawWorld g <+> padLeft (Pad 2) (drawInventory (g ^. inventory))]

robotLocs :: (Traversable t) => Traversal' (t Robot) (V2 Int, V2 Int)
robotLocs = traverse . p
  where 
    p :: Lens' Robot (V2 Int, V2 Int)
    p = lensProduct location direction 

robotLocs' :: Traversal' [Robot] (V2 Int, V2 Int)
robotLocs' = robotLocs

drawWorld :: GameState -> Widget Name
drawWorld g = withBorderStyle BS.unicode
  $ B.border
  $ padAll 1
  $ vBox (imap (\r -> hBox . imap (\c x -> drawLoc r c x)) (g ^. world))
  where
    robotLocs = M.fromList $ g ^.. (robots . (traverse . lensProduct location direction))
    drawLoc r c x = case M.lookup (V2 r c) robotLocs of
      Just dir -> withAttr robotAttr $ str (robotDir dir)
      Nothing  -> str [x]

robotDir (V2 0 1)    = "▶"
robotDir (V2 0 (-1)) = "◀"
robotDir (V2 1 0)    = "▼"
robotDir (V2 (-1) 0) = "▲"

drawInventory :: Map Item Int -> Widget Name
drawInventory inv = withBorderStyle BS.unicode
  $ B.borderWithLabel (str "Inventory")
  $ padAll 1
  $ vLimit 10
  $ padBottom Max
  $ vBox
  $ map drawItem (M.assocs inv)

drawItem :: (Item, Int) -> Widget Name
drawItem (Resource c, n) = padRight (Pad 1) (str [c]) <+> showCount n
  where
    showCount = hLimit 7 . padLeft Max . str . show

------------------------------------------------------------

testGameState :: GameState
testGameState = GameState [] [Robot (V2 0 0) (V2 0 1) testProgram] ["*.*$", "%**a"] M.empty

testProgram :: Program
testProgram = [Wait, Harvest, Move, Harvest, TR, Move, Harvest, TL, Move, Harvest, Harvest]

main :: IO ()
main = do
  chan <- newBChan 10
  forkIO $ forever $ do
    writeBChan chan Tick
    threadDelay 500000 -- decides how fast your game moves
  let buildVty = V.mkVty V.defaultConfig
  vty <- buildVty
  void $ customMain vty buildVty (Just chan) app testGameState
