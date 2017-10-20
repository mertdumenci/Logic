{-# LANGUAGE OverloadedStrings #-}
module Data.Ord.Graph.Extras where

import           Control.Lens
import           Control.Monad.State

import           Data.Ord.Graph
import           Data.Maybe (fromJust)
import qualified Data.Map as M
import           Data.Monoid ((<>))

import           Prelude hiding (reverse)

import qualified Turtle
import           Text.PrettyPrint.HughesPJClass (Pretty, prettyShow)

backEdges :: Ord i => Graph i e v -> [((i, i), e)]
backEdges g = filter (\((i1, i2), _) -> i2 <= i1) $ g ^@.. iallEdges

unfold :: (Applicative f, Ord i)
       => (i -> f i)
       -> (i -> i -> e -> f e)
       -> (i -> v -> f v)
       -> i -> i -> e
       -> Graph i e v -> f (Graph i e v)
unfold fi fe fv i1 i2 e g =
  let g' = reaches i1 g
      am = M.fromList <$> traverse (\i -> fmap (\i' -> (i, i')) (fi i)) (idxs g')
      ae = fe i1 i2 e
      ag = idfs fe fv g'
  in complete i1 i2 <$> am <*> ae <*> pure g <*> ag

  where
    complete i1 i2 m e g g' = delEdge i1 i2 (connect m e g (unfold' m g'))
    unfold' m = mapIdxs (\i -> M.findWithDefault i i m)
    connect m e g g' = addEdge (M.findWithDefault i1 i1 m) i2 e $ union g g'

display :: (MonadIO m, Eq i, Pretty i, Pretty e, Pretty v)
        => FilePath -> Graph i e v -> m ()
display fn g = do
  let txt = dot ": " prettyShow prettyShow prettyShow g
  liftIO $ writeFile fn txt
  let fn' = Turtle.fromString fn
  _ <- Turtle.shell ("dot -Tpdf " <> fn' <> "> " <> fn' <> ".pdf") Turtle.empty
  _ <- Turtle.shell ("open " <> fn' <> ".pdf") Turtle.empty
  return ()

dot :: Eq i
    => String -> (i -> String) -> (e -> String) -> (v -> String)
    -> Graph i e v -> String
dot sep fi fe fv g =
  let vs = map showVert (g ^@.. iallVerts)
      es = map showEdge (g ^@.. iallEdges)
  in
    "digraph {\n" ++ unlines (map ("  " ++) (vs ++ es)) ++ "}"
  where
    lbls = zip (idxs g) [0..]
    lbl i = show $ fromJust $ lookup i lbls

    showVert (i, v) =
      lbl i ++ " [label=\"" ++ fi i ++ sep ++ fv v ++ "\"];"
    showEdge ((i1, i2), e) =
      lbl i1 ++ " -> " ++ lbl i2 ++ " [label=\"" ++ fe e ++ "\"];"
