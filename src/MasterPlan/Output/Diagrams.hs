module MasterPlan.Output.Diagrams where

import MasterPlan.Project

data RenderOptions = RenderOptions
  deriving (Eq, Ord)

renderProject :: Project -> RenderOptions -> IO ()
renderProject = error "Not implemented: renderProject"
