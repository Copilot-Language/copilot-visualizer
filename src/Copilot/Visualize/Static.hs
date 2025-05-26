-- | Graphical visualization of Copilot specifications.
--
-- This module contains an abstract representation of a trace as a series
-- named streams with their values, together with functions needed to
-- convert a 'Spec' into a trace.
--
-- The helper function 'makeTrace' expands a latex file that produces the
-- trace visualization as a figure.
module Copilot.Visualize.Static
    ( makeTrace
    )
  where

-- External imports
import Data.Aeson             (ToJSON (..))
import System.Directory.Extra (copyTemplate)
import System.FilePath        ((</>))

-- External imports: Copilot
import Copilot.Core           (Spec (..))
import Copilot.Interpret.Eval (ShowType (Haskell), eval)

-- Internal imports
import Copilot.Visualize.UntypedTrace (makeTraceEval)
import Paths_copilot_visualizer       (getDataDir)

-- | Generate a visualization of a specification for a given number of steps.
makeTrace :: Int     -- ^ Number of steps to interpret.
          -> Spec    -- ^ Specification to interpret.
          -> String  -- ^ Base used to expand the static file (i.e., @"tikz"@,
                     -- @"static_html"@).
          -> IO ()
makeTrace k spec base = do
  dir <- getDataDir
  let f = dir </> "data" </> base
  let subs = toJSON $ makeTraceEval k spec $ eval Haskell k spec
  copyTemplate f subs "target"
