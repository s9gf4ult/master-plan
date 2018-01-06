{-|
Module      : Main
Description : Parses command line and dispatches to correct backend
Copyright   : (c) Rodrigo Setti, 2017
License     : MIT
Maintainer  : rodrigosetti@gmail.com
Stability   : experimental
Portability : POSIX
-}
{-# LANGUAGE UnicodeSyntax #-}
module Main (main) where

import MasterPlan.Cli
import Options.Applicative

main ∷ IO ()
main = masterPlan =<< execParser opts
