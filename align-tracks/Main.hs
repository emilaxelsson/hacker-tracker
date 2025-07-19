module Main (main) where

import CMark (PosInfo (..))
import Data.HashMap.Strict qualified as HM
import Data.Text qualified as Text
import Oops
import Protolude
import System.FilePath (takeExtension)
import Track.AST
import Track.Parser
import Track.PrettyPrinter (prettyNote)
import Prelude (fail)

data DocumentPart
    = CodeBlock Int [Text]
    | Other [Text]
    deriving stock (Show)

mapCode :: ([Text] -> [Text]) -> [DocumentPart] -> [DocumentPart]
mapCode f = map $ \case
    CodeBlock n ls -> CodeBlock n $ f ls
    p -> p

-- | Recognize a Markdown code block fence line and return its width
--
-- >>> fenceLine "abc"
-- Nothing
--
-- >>> fenceLine "```"
-- Just 3
--
-- >>> fenceLine "`"
-- Nothing
fenceLine :: Text -> Maybe Int
fenceLine t
    | len >= 3 && Text.all ('`' ==) t = Just len
    | otherwise = Nothing
  where
    len = Text.length t

-- | Recognize a Markdown code block fence line of length @n@
--
-- >>> fenceLineN 4 "````"
-- True
--
-- >>> fenceLineN 4 "`````"
-- False
--
-- >>> fenceLineN 4 "abc"
-- False
fenceLineN :: Int -> Text -> Bool
fenceLineN n t = isJust (fenceLine t) && Text.length t == n

-- |
-- >>> splitParts "Sun is shining\nBirds are singing"
-- [Other ["Sun is shining","Birds are singing"]]
--
-- >>> splitParts "The code:\n```\nmain = print ()\n```\nAnd the rest of the text."
-- [Other ["The code:"],CodeBlock 3 ["main = print ()"],Other ["And the rest of the text."]]
--
-- >>> splitParts "The code:\n```\nOpen code block\n"
-- [Other ["The code:"],CodeBlock 3 ["Open code block"]]
--
-- >>> splitParts "`````\nCode with ``` inside\n`````\n"
-- [CodeBlock 5 ["Code with ``` inside"]]
splitParts :: Text -> [DocumentPart]
splitParts = goOther [] . Text.lines
  where
    goOther parts [] = reverse parts
    goOther parts (l : ls)
        | Just n <- fenceLine l =
            let (block, rest) = break (fenceLineN n) ls
                rest' = case rest of
                    l2 : ls2
                        | fenceLineN n l2 -> ls2
                    _ -> rest
             in goOther (CodeBlock n block : parts) rest'
    goOther parts ls = goOther (Other other : parts) rest
      where
        (other, rest) = break (isJust . fenceLine) ls

-- |
-- >>> joinParts [Other ["abc"], CodeBlock 3 ["def"]]
-- "abc\n```\ndef\n```\n"
joinParts :: [DocumentPart] -> Text
joinParts = Text.unlines . concatMap partLines
  where
    partLines (CodeBlock n ls) = [fence] ++ ls ++ [fence]
      where
        fence = Text.replicate n "`"
    partLines (Other ls) = ls

-- | Pad the cells with spaces so that all cells in each column have the same width. The
-- argument is assumed to be rectangular (all rows have the same length).
alignGrid :: [[Text]] -> [[Text]]
alignGrid rows = transpose paddedCols
  where
    cols = transpose rows
    paddedCols =
        [ [Text.take w $ r <> Text.replicate w " " | r <- col]
        | col <- cols
        , let w = maximumDef 0 (map Text.length col) + 2
        ]

align :: [InstrumentAcr] -> [Text] -> [Text]
align knownInstruments rows = map Text.unwords $ alignGrid sortedRows
  where
    nullPos = PosInfo 0 0 0 0

    -- Not passing `knownInstruments` to `parseRow`, because we must be able to handle
    -- unknown instruments as well
    parsedRows =
        [ either (oops "") notes $ parseRow Nothing (nullPos, row)
        | row <- rows
        ]

    rowsWithNotesByInstrument =
        map (\row -> HM.fromList [(instrument n, n) | n <- row]) parsedRows

    foundInstruments = concatMap (map instrument) parsedRows

    -- Order of instruments is important here; we respect the order in `knownInstruments`
    -- and take any others as they appear in the tracks
    allInstruments = ordNub $ knownInstruments ++ foundInstruments

    sortedRows =
        [ [maybe "*" prettyNote $ HM.lookup instr row | instr <- allInstruments]
        | row <- rowsWithNotesByInstrument
        ]

-- | Align the instruments in each pattern (i.e. code block). The columns are sorted
-- according to the order in which the instruments are configured in the track.
alignTracks :: FilePath -> IO ()
alignTracks path = do
    unless (takeExtension path == ".md") $
        fail "File must have extension .md"
    contents <- readFile path

    TrackConfig{instruments = instrMap} <-
        either (fail . show) return $ parseTrackConfig contents
    let instruments = map fst instrMap

    let parts = splitParts contents
    let alignedParts = mapCode (align instruments) parts

    writeFile path $ joinParts alignedParts

main :: IO ()
main = do
    args <- getArgs
    case args of
        [path] -> alignTracks path
        _ -> fail "expected a single file name as argument"
