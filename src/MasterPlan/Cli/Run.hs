module MasterPlan.Cli.Run where

import Control.Lens
import MasterPlan.Cli.Opts
import MasterPlan.Input.Yaml
import MasterPlan.Output.Diagrams
import MasterPlan.Project

masterPlan ∷ Opts → IO ()
masterPlan opts = do
  modules <- loadYamlModules $ opts ^. oInputPath
  project <- do
    p <- interpretModules modules $ opts ^. oParsingOpts
    return $ bestVariant $ calculateProject p
  renderProject project $ opts ^. oRenderOptions
