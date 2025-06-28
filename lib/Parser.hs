module Parser where

import CMark qualified as MD
import Data.HashMap.Strict qualified as HM
import Data.List qualified as List
import Data.Text qualified as Text
import Oops
import Protolude
import Types
import Utils

type LocatedError = (Maybe MD.PosInfo, Text)

locatedError :: MD.Node -> Text -> LocatedError
locatedError (MD.Node pos _ _) msg = (pos, msg)

parseSong :: Text -> Either LocatedError Song
parseSong = mdToSong . MD.commonmarkToNode []

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

data ConfigItem
    = ConfigBPM Int
    | ConfigInstr InstrumentAcr InstrumentTarget
    deriving stock (Show)

parseConfig :: Text -> Maybe ConfigItem
parseConfig item = do
    let (key', value') = Text.break (== ':') item
    let key = Text.strip key'
    let value = Text.strip $ Text.dropWhile (== ':') value'

    case Text.map toLower key of
        "bpm" -> ConfigBPM <$> readMaybe value
        "instr" -> do
            [acr, tgt] <- return $ Text.words value
            guard $ Text.length acr < 10
            guard $ Text.all isUpper acr
            target <- readMaybe tgt
            return $ ConfigInstr (InstrumentAcr acr) (InstrumentTarget target)
        _ -> Nothing

getConfigItem :: MD.Node -> Either LocatedError ConfigItem
getConfigItem (MD.Node _ MD.ITEM [MD.Node _ MD.PARAGRAPH [MD.Node _ (MD.TEXT item) []]])
    | Just conf <- parseConfig item = return conf
getConfigItem n = Left $ locatedError n "Ill-formatted config item."

getConfig :: [MD.Node] -> Either LocatedError SongConfig
getConfig [] = Left (Nothing, "Missing song config.")
getConfig (n : _ : _) = Left $ locatedError n "Config section should consist of a single bullet list."
getConfig [config] = case config of
    MD.Node _ (MD.LIST (MD.ListAttributes MD.BULLET_LIST _ _ _)) items -> do
        cs <- mapM getConfigItem items

        bpm <- first (locatedError config) $ case [bpm | ConfigBPM bpm <- cs] of
            [] -> Left "Missing BPM config."
            _ : _ : _ -> Left "Multiple BPM configs."
            [b] -> return b

        let instrAcrs = [acr | ConfigInstr acr _ <- cs]
        let dups = List.nub (instrAcrs List.\\ List.nub instrAcrs)
        when (not $ null dups) $
            Left $
                locatedError config $
                    "Multiple definitions of instruments: " <> show dups
        let instruments = HM.fromList [(a, t) | ConfigInstr a t <- cs]

        return $ SongConfig{bpm, instruments}
    n -> Left $ locatedError n "Config section should be a bullet list."

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

getSongSections :: [((MD.PosInfo, Text), [MD.Node])] -> Either LocatedError [Section]
getSongSections [] = return []
getSongSections (((_, sectionTitle), nodes) : ss) = do
    let (lead, h2s) = splitOn (either (const Nothing) Just . getHeading 2) nodes
    case lead of
        l@(MD.Node _ typ _) : _ ->
            Left $
                locatedError l $
                    "Song section must begin with a pattern (i.e. a level-2 heading). Got " <> show typ
        _ -> return ()
    patterns <- getSectionPatterns h2s
    let section = Section{sectionTitle, patterns}
    (section :) <$> getSongSections ss

mdToSong :: MD.Node -> Either LocatedError Song
mdToSong (MD.Node _ typ nodes)
    | typ /= MD.DOCUMENT = oops $ toS $ unexpectedNodeType MD.DOCUMENT typ
    | otherwise = do
        config <- getConfig intro
        sections <- getSongSections h1s
        return $ Song{config, sections}
  where
    (intro, h1s) = splitOn (either (const Nothing) Just . getHeading 1) nodes
