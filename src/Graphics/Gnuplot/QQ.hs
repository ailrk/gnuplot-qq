{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}


module Graphics.Gnuplot.QQ where

import Data.Foldable (Foldable(..))
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import System.IO (hClose)
import System.IO.Temp (withSystemTempFile)
import System.Process
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Language.Haskell.Meta.Parse
import Text.Regex.TDFA ((=~))


type DataSetName = Text


data DataSet = DataSet
  { name :: DataSetName
  , rows :: [[Double]]
  }


data Item
  = DataItem DataSet
  | InterpItem (String, String)


data Gnuplot = Gnuplot { script :: Text, items :: [Item] }


gnuplot :: QuasiQuoter
gnuplot = QuasiQuoter
  { quoteExp  = quoteGnuplotExp
  , quotePat  = error "can't use gnuplot on patterns"
  , quoteType = error "can't use gnuplot on patterns"
  , quoteDec  = error "can't use gnuplot on patterns"
  }


normaliseNewlines :: String -> String
normaliseNewlines []             = []
normaliseNewlines ('\r':'\n':cs) = '\n':normaliseNewlines cs
normaliseNewlines (c:cs)         = c:normaliseNewlines cs


quoteGnuplotExp :: String -> Q Exp
quoteGnuplotExp s = foldr appendChunk [| Gnuplot "" [] |] parts
  where
    parts = interpolate . normaliseNewlines $ s


interpolate :: String -> [Either String String]
interpolate "" = []
interpolate s =
  case s =~ Text.pack "#" :: (String, String, String, [String]) of
  -- case s =~ Text.pack "(.*?)#\\{([^}]+)\\}" :: (String, String, String, [String]) of
    (before, "", "", []) -> [Left before]
    (before, _, after, [expr]) -> Left before : Right expr : interpolate after
    _ -> [Left s]


appendChunk :: Either String String -> Q Exp -> Q Exp
appendChunk (Left txt) acc = [| let (Gnuplot old items) = $acc in Gnuplot (Text.pack txt <> old) items |]
appendChunk (Right expr) acc = do
  case parseExp expr of
    Right e -> [| let (Gnuplot old ds) = $acc in Gnuplot (Text.pack (show $(pure e)) <> old) ds |]
    Left err -> fail ("Failed to parse expression: " ++ err)


dataSetToText :: [[Double]] -> Text
dataSetToText rows = Text.unlines $ fmap rowToText rows
  where
    rowToText = Text.unwords . fmap (Text.pack . show)


withTempFiles :: [DataSet] -> ([(DataSetName, FilePath)] -> IO a) -> IO a
withTempFiles dataSets action = go dataSets []
  where
    go [] acc = action acc
    go ((DataSet name rows):rest) acc =
      withSystemTempFile (Text.unpack name ++ ".dat") $ \fp h -> do
        Text.hPutStr h (dataSetToText rows)
        hClose h
        go rest ((name, fp):acc)


runGnuplot :: Gnuplot -> [DataSet] -> IO ()
runGnuplot (Gnuplot script _) dataSets = do
  withTempFiles dataSets \mapping -> do
    withSystemTempFile "gnuplot.gpl" \gpl gplH -> do
      let finalScript = foldl' replace script mapping
      Text.hPutStr gplH finalScript
      hClose gplH
      (_, _, _, ph) <- createProcess (proc "gnuplot" [gpl])
        { std_out = Inherit
        , std_err = Inherit
        }
      _ <- waitForProcess ph
      pure ()
  where
    replace text (name, path) = Text.replace ("{{"<> name <> "}}") ("'" <> Text.pack path <> "'") text
