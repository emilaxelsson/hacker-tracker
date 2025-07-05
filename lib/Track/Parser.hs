module Track.Parser where

import CMark qualified as MD
import Data.HashMap.Strict qualified as HM
import Data.List qualified as List
import Data.Text qualified as Text
import Oops
import Protolude
import Track.Types
import Utils

type LocatedError = (Maybe MD.PosInfo, Text)

locatedError :: MD.Node -> Text -> LocatedError
locatedError (MD.Node pos _ _) msg = (pos, msg)

parseTrack :: Text -> Either LocatedError Track
parseTrack = mdToTrack . MD.commonmarkToNode []

unexpectedNodeType :: MD.NodeType -> MD.NodeType -> Text
unexpectedNodeType expected actual =
    mconcat ["Expected ", show expected, " got ", show actual, "."]

guardHeading :: Int -> MD.NodeType -> Either Text ()
guardHeading expectedLevel (MD.HEADING actualLevel)
    | expectedLevel == actualLevel = return ()
guardHeading expectedLevel actual = Left $ unexpectedNodeType (MD.HEADING expectedLevel) actual

getHeading :: Int -> MD.Node -> Either LocatedError (MD.PosInfo, Text)
getHeading level n@(MD.Node pos typ [MD.Node _ (MD.TEXT h) []]) =
    first (locatedError n) $ do
        guardHeading level typ
        return (fromMaybe (oops "heading has no position") pos, h)
getHeading level n =
    first (locatedError n) $
        Left $
            mconcat ["Ill-formatted level-", show level, " heading."]

parseConfigItem :: Text -> Maybe (Text, Text)
parseConfigItem item = do
    let (key', value') = Text.break (== ':') item
    let key = Text.strip key'
    let value = Text.strip $ Text.dropWhile (== ':') value'
    return (Text.map toLower key, value)

getConfigItem :: MD.Node -> Either LocatedError (Text, Text)
getConfigItem (MD.Node _ MD.ITEM [MD.Node _ MD.PARAGRAPH [MD.Node _ (MD.TEXT item) []]])
    | Just conf <- parseConfigItem item = return conf
getConfigItem n = Left $ locatedError n "Ill-formatted config item."

-- | Checks for duplicate config keys
getConfigItems
    :: (Text -> Text -> Maybe configItem)
    -- ^ Parse a key and a value to a config item. Key is always in lower-case letters.
    -> MD.Node
    -> Either LocatedError [configItem]
getConfigItems itemParser config = case config of
    MD.Node _ (MD.LIST (MD.ListAttributes MD.BULLET_LIST _ _ _)) items -> do
        cs <- mapM getConfigItem items
        for cs $ \(key, value) ->
            maybe (Left $ locatedError config $ "Cannot parse config item " <> show key) Right $
                itemParser key value
    n -> Left $ locatedError n "Config section should be a bullet list."

data TrackConfigItem
    = TrackConfigBPM Int
    | TrackConfigInstr InstrumentAcr InstrumentTarget
    deriving stock (Show)

parseTrackConfigItem :: Text -> Text -> Maybe TrackConfigItem
parseTrackConfigItem "bpm" value = TrackConfigBPM <$> readMaybe value
parseTrackConfigItem "instr" value = do
    [acr, tgt] <- return $ Text.words value
    guard $ Text.length acr < 10
    guard $ Text.all isUpper acr
    target <- readMaybe tgt
    return $ TrackConfigInstr (InstrumentAcr acr) (InstrumentTarget target)
parseTrackConfigItem _ _ = Nothing

getTrackConfig :: [MD.Node] -> Either LocatedError TrackConfig
getTrackConfig [] = Left (Nothing, "Missing track config section.")
getTrackConfig (n : _ : _) = Left $ locatedError n "Track config section should consist of a single bullet list."
getTrackConfig [config] = do
    cs <- getConfigItems parseTrackConfigItem config

    bpm <- first (locatedError config) $ case [bpm | TrackConfigBPM bpm <- cs] of
        [] -> Left "Missing BPM config."
        _ : _ : _ -> Left "Multiple BPM configs."
        [b] -> return b

    let instrAcrs = [acr | TrackConfigInstr acr _ <- cs]
    let dups = List.nub (instrAcrs List.\\ List.nub instrAcrs)
    unless (null dups) $
        Left $
            locatedError config $
                "Multiple definitions of instruments: " <> show dups
    let instruments = HM.fromList [(a, t) | TrackConfigInstr a t <- cs]

    return $ TrackConfig{bpm, instruments}

getSectionPatterns :: [((MD.PosInfo, Text), [MD.Node])] -> Either LocatedError [Pattern]
getSectionPatterns [] = return []
getSectionPatterns (((pos@MD.PosInfo{startLine}, patternTitle), nodes) : ss) = do
    (_config, _patternNodes) <- case nodes of
        [c, p] -> return (c, p)
        _ ->
            Left
                ( Just pos
                , "Pattern section must consist of exactly two parts: an initial bullet list (config) and a code block (the pattern). Got "
                    <> show (length nodes)
                    <> " nodes."
                )
    let resolution = 2 :: Int
    let rows = []
    let pattern =
            Pattern
                { patternSourceLine = startLine
                , patternTitle
                , resolution
                , rows
                }
    (pattern :) <$> getSectionPatterns ss

getTrackSections :: [((MD.PosInfo, Text), [MD.Node])] -> Either LocatedError [Section]
getTrackSections [] = return []
getTrackSections (((_, sectionTitle), nodes) : ss) = do
    let (lead, h2s) = splitOn (either (const Nothing) Just . getHeading 2) nodes
    case lead of
        l@(MD.Node _ typ _) : _ ->
            Left $
                locatedError l $
                    "Track section must begin with a pattern (i.e. a level-2 heading). Got " <> show typ
        _ -> return ()
    patterns <- getSectionPatterns h2s
    let section = Section{sectionTitle, patterns}
    (section :) <$> getTrackSections ss

mdToTrack :: MD.Node -> Either LocatedError Track
mdToTrack (MD.Node _ typ nodes)
    | typ /= MD.DOCUMENT = oops $ toS $ unexpectedNodeType MD.DOCUMENT typ
    | otherwise = do
        config <- getTrackConfig intro
        sections <- getTrackSections h1s
        return $ Track{config, sections}
  where
    (intro, h1s) = splitOn (either (const Nothing) Just . getHeading 1) nodes
