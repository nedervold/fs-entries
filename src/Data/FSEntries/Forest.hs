-- | Conversion to and from rose-tree forests from "Data.Tree".
module Data.FSEntries.Forest
  ( -- * Conversions
    entriesToForest
  , forestToEntries
    -- * ASCII drawings
  , drawFSEntries
  , drawFSStructure
  ) where

import Data.Bifunctor (Bifunctor(..))
import Data.FSEntries.Types
import qualified Data.Map as M
import Data.Tree (Forest, Tree(..), drawTree)
import Text.Printf (printf)

-- | Convert an 'FSEntries' to a 'Forest'.
entriesToForest :: FSEntries d f -> Forest (String, Either d f)
entriesToForest = map pairToTree . M.toList . unFSEntries

pairToTree :: (String, FSEntry d f) -> Tree (String, Either d f)
pairToTree (filename, File f) = Node (filename, Right f) []
pairToTree (filename, Dir d entries) =
  Node (filename, Left d) $ entriesToForest entries

-- | Convert a 'Forest' to an 'FSEntries'.
forestToEntries :: Forest (String, Either d f) -> FSEntries d f
forestToEntries = mkFSEntries . map treeToPair

treeToPair :: Tree (String, Either d f) -> (String, FSEntry d f)
treeToPair (Node (name, Left d) forest) =
  (name, Dir d $ forestToEntries forest)
treeToPair (Node (name, Right f) forest) =
  if null forest
    then (name, File f)
    else error "treeToPair: file Node has descendants"

-- | A two-dimensional ASCII drawing of an 'FSEntries'.  A wrapper
-- around 'Data.Tree.drawForest'.
drawFSEntries :: (d -> String) -> (f -> String) -> FSEntries d f -> String
drawFSEntries showD showF entries = drawTree (Node "/:" forest')
  where
    forest :: Forest (String, Either String String)
    forest = entriesToForest $ bimap showD showF entries
    forest' :: Forest String
    forest' = fmap (fmap f) forest
      where
        f :: (String, Either String String) -> String
        f (filename, Left str) = printf "%s/: %s" filename str
        f (filename, Right str) = printf "%s: %s" filename str

-- | A two-dimensional ASCII drawing of the structure of an
-- 'FSEntries'.  Ignores the directory and file data.  A wrapper
-- around 'Data.Tree.drawForest'.
drawFSStructure :: FSEntries d f -> String
drawFSStructure = drawFSEntries (const "<dir>") (const "<file>")
