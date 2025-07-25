module Track.Parser
    ( LocatedError
    , parseNote
    , parseRow
    , parseTrack
    , parseTrackConfig
    ) where

import CMark qualified as MD
import Data.Char (isNumber)
import Data.Text qualified as Text
import Oops
import Protolude hiding (note)
import Text.ParserCombinators.ReadP qualified as Parse
import Track.AST
import Utils

type LocatedError = (Maybe MD.PosInfo, Text)

locatedError :: MD.Node -> Text -> LocatedError
locatedError (MD.Node pos _ _) msg = (pos, msg)

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

    bpm <- fmap BPM $
        first (locatedError config) $ case [bpm | TrackConfigBPM bpm <- cs] of
            [] -> Left "Missing BPM config."
            _ : _ : _ -> Left "Multiple BPM configs."
            [b] -> return b

    let instruments = [(a, t) | TrackConfigInstr a t <- cs]

    return $ TrackConfig{bpm, instruments}

data SectionConfigItem
    = SectionConfigResolution Int
    deriving stock (Show)

parseSectionConfigItem :: Text -> Text -> Maybe SectionConfigItem
parseSectionConfigItem "resolution" value =
    SectionConfigResolution <$> do
        resolution <- readMaybe value
        guard $ resolution > 0
        return resolution
parseSectionConfigItem _ _ = Nothing

pitchParser :: Parse.ReadP Pitch
pitchParser = do
    name <- Parse.get
    raised <- Parse.option False (True <$ Parse.char '#')
    oct <- Parse.option Nothing (Just <$> Parse.get)
    Parse.eof

    let octave = oct <&> \o -> ord o - ord '0'

    for_ octave $ \o -> do
        guard $ o >= 0
        guard $ o <= 8

    noteName <- case (name, raised) of
        ('A', False) -> return A
        ('A', True) -> return As
        ('B', False) -> return B
        ('C', False) -> return C
        ('C', True) -> return Cs
        ('D', False) -> return D
        ('D', True) -> return Ds
        ('E', False) -> return E
        ('F', False) -> return F
        ('F', True) -> return Fs
        ('G', False) -> return G
        ('G', True) -> return Gs
        _ -> Parse.pfail

    return Pitch{noteName, octave}

instrParser :: Parse.ReadP InstrumentAcr
instrParser =
    fmap (InstrumentAcr . Text.pack) $ Parse.munch1 $ \c -> c >= 'A' && c <= 'Z'

velocityParser :: Parse.ReadP Velocity
velocityParser = do
    n <- Parse.munch1 isNumber
    Just v <- return $ readMaybe n
    return $ Velocity v

noteParser :: MD.PosInfo -> Parse.ReadP Note
noteParser noteSourcePos = do
    instrument <- instrParser
    let genericNote = Note{noteSourcePos, instrument, velocity = Nothing, pitch = Nothing}

    let withVelocityAndPitch = do
            void $ Parse.char '-'
            velocity <- Just <$> velocityParser
            void $ Parse.char '-'
            pitch <- Just <$> pitchParser
            return $ genericNote{velocity, pitch}

    let withVelocity = do
            void $ Parse.char '-'
            velocity <- Just <$> velocityParser
            return $ genericNote{velocity}

    let withPitch = do
            void $ Parse.char '-'
            pitch <- Just <$> pitchParser
            return $ genericNote{pitch}

    let onlyInstrument = Parse.eof >> return genericNote

    withVelocityAndPitch Parse.<++ withVelocity Parse.<++ withPitch Parse.<++ onlyInstrument

parseNote :: MD.PosInfo -> Text -> Either LocatedError Note
parseNote pos word =
    case Parse.readP_to_S (noteParser pos) $ Text.unpack word of
        [(note, "")] -> Right note
        _ -> Left (Just pos, "Cannot parse note: '" <> word <> "'")

parseRow :: (MD.PosInfo, Text) -> Either LocatedError (Row Note)
parseRow (pos@MD.PosInfo{startLine}, line) = do
    let ws = filter (\w -> not (Text.null w) && w /= "*") $ Text.words line
    notes <- mapM (parseNote pos) ws
    return $ Row{rowSourceLine = fromIntegral startLine, notes}

getRows :: MD.Node -> Either LocatedError [Row Note]
getRows (MD.Node mpos (MD.CODE_BLOCK _ block) _) = do
    let pos = fromMaybe (oops "missing source location") mpos
    let ls = Text.lines block
    let positionedLines =
            [ (pos', line)
            | (i, line) <- zip [1 ..] ls -- From 1 because the real block starts after the fence line
            , let pos' =
                    pos
                        { MD.startLine = MD.startLine pos + i
                        , MD.endLine = MD.startLine pos + i
                        , MD.startColumn = 0
                        , MD.endColumn = Text.length line - 1
                        }
            ]
    mapM parseRow positionedLines
getRows n = Left $ locatedError n "Expected code block."

getSectionPatterns
    :: [((MD.PosInfo, Text), [MD.Node])]
    -> Either LocatedError [Pattern [] Note]
getSectionPatterns [] = return []
getSectionPatterns (((pos, patternTitle), nodes) : ss) = do
    (config, patternNode) <- case nodes of
        [c, p] -> return (c, p)
        _ ->
            Left
                ( Just pos
                , "Pattern section must consist of exactly two parts: an initial bullet list (config) and a code block (the pattern). Got "
                    <> show (length nodes)
                    <> " nodes."
                )

    cs <- getConfigItems parseSectionConfigItem config

    resolution <- fmap Resolution $
        first (locatedError config) $ case [resolution | SectionConfigResolution resolution <- cs] of
            [] -> Left "Missing resolution config."
            _ : _ : _ -> Left "Multiple resolution configs."
            [r] -> return r

    rows <- getRows patternNode

    let pattern =
            Pattern
                { patternSourcePos = pos
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

mdToConfig :: MD.Node -> Either LocatedError TrackConfig
mdToConfig (MD.Node _ typ nodes)
    | typ /= MD.DOCUMENT = oops $ toS $ unexpectedNodeType MD.DOCUMENT typ
    | otherwise = getTrackConfig intro
  where
    (intro, _) = splitOn (either (const Nothing) Just . getHeading 1) nodes

parseTrack :: Text -> Either LocatedError Track
parseTrack = mdToTrack . MD.commonmarkToNode []

-- | Parse only the initial config section of a track
parseTrackConfig :: Text -> Either LocatedError TrackConfig
parseTrackConfig = mdToConfig . MD.commonmarkToNode []
