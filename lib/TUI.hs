{-# LANGUAGE ViewPatterns #-}

module TUI
    ( AppEvent (..)
    , PlayerCommand (..)
    , trackerMain
    ) where

import Brick
    ( App (..)
    , BrickEvent (AppEvent, VtyEvent)
    , EventM
    , Padding (..)
    , Size (..)
    , Widget (..)
    , hBox
    , halt
    , padRight
    , translateBy
    , txt
    )
import Brick.AttrMap (attrMap)
import Brick.BChan (BChan)
import Brick.Main (customMain, neverShowCursor)
import Brick.Types (Location (..))
import Brick.Widgets.Border (border)
import Graphics.Vty (Event (..), Key (..), defaultConfig)
import Graphics.Vty.Attributes (defAttr)
import Graphics.Vty.Platform.Unix (mkVty)
import Protolude hiding (Location)

type TrackPosition = Text

-- | Incoming event to the TUI
data AppEvent
    = NowPlaying TrackPosition

-- | Command from the TUI to the player
data PlayerCommand
    = Start
    | Stop

data AppState = AppState
    { playing :: Bool
    , trackPosition :: Maybe TrackPosition
    }

initState :: AppState
initState =
    AppState
        { playing = False
        , trackPosition = Nothing
        }

drawApp :: AppState -> [Widget n]
drawApp AppState{playing, trackPosition} =
    [ translateBy (Location (3, 0)) title
    , border $
        hBox
            [ playSymbolWidget
            , separatorWidget
            , padRight Max playingWidget
            ]
    ]
  where
    title = Widget Fixed Fixed $ do
        render $ txt " hacker-tracker "

    playSymbolWidget = Widget Fixed Fixed $ do
        render $ txt $ if playing then "▶" else "■"

    separatorWidget = Widget Fixed Fixed $ render $ txt "  "

    playingWidget = Widget Fixed Fixed $ do
        render $ txt $ fromMaybe "---" trackPosition

-- | Case-insensitive key-press recognizer
keyPressed :: Char -> BrickEvent n e -> Bool
keyPressed c (VtyEvent (EvKey (KChar c') [])) = toLower c == toLower c'
keyPressed _ _ = False

stepState :: BrickEvent n AppEvent -> AppState -> AppState
stepState (VtyEvent (EvKey (KChar c) [])) s@AppState{playing}
    | c == ' ' = s{playing = not playing}
stepState (AppEvent (NowPlaying pos)) s = s{trackPosition = Just pos}
stepState _ s = s

stepApp' :: BrickEvent n AppEvent -> EventM n AppState ()
stepApp' (keyPressed 'q' -> True) = halt
stepApp' ev = modify $ stepState ev

stepApp
    :: (PlayerCommand -> IO ())
    -> BrickEvent n AppEvent
    -> EventM n AppState ()
stepApp player ev = do
    s <- get
    stepApp' ev
    s' <- get
    when (playing s /= playing s') $
        liftIO $
            if playing s'
                then player Start
                else player Stop

trackerApp
    :: (PlayerCommand -> IO ())
    -> App AppState AppEvent ()
trackerApp player =
    App
        { appDraw = drawApp
        , appHandleEvent = stepApp player
        , appStartEvent = return ()
        , appAttrMap = const $ attrMap defAttr []
        , appChooseCursor = neverShowCursor
        }

trackerMain
    :: BChan AppEvent
    -> (PlayerCommand -> IO ())
    -> IO AppState
trackerMain eventChan player = do
    vty <- mkVty defaultConfig
    customMain
        vty
        (mkVty defaultConfig)
        (Just eventChan)
        (trackerApp player)
        initState
