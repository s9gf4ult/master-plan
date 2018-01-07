module MasterPlan.Cli.Run where

import Control.Lens
import MasterPlan.Cli.Opts
import MasterPlan.Input.Yaml

masterPlan ∷ Opts → IO ()
masterPlan opts = do
  modules <- loadYamlModules $ opts ^. oInputPath
  projects <- interpretModules modules $ opts ^. oParsingOpts
  (error "FIXME: do something")

    -- do contents <- maybe (TIO.hGetContents stdin) TIO.readFile $ inputPath opts
    --    let outfile = fromMaybe (fromMaybe "output" (outputPath opts) ++ ".pdf") $ outputPath opts
    --    case P.runParser (parseStrict opts) (fromMaybe "stdin" $ inputPath opts) contents (rootKey opts) of
    --       Left e    -> if renderParsingError opts
    --                     then renderText outfile (renderOptions opts) (lines e)
    --                     else die e
    --       Right p ->
    --         do let p' = fromMaybe defaultAtomic $ prioritize <$> filterProj (projFilter opts) p
    --            render outfile (renderOptions opts) p'
