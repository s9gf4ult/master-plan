module MasterPlan.Internal.TH where

import Data.Aeson.TH
import Data.Char
import Data.Text as T
import Text.Inflections

camelCaseNoPrefix :: String -> String
camelCaseNoPrefix name = case toUnderscore $ T.dropWhile (not . isUpper) $ T.pack name of
  Left e    -> error $ show e
  Right res -> T.unpack res

jsonOpts :: Options
jsonOpts = defaultOptions
  { fieldLabelModifier = camelCaseNoPrefix
  , omitNothingFields  = True
  }
