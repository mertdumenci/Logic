{-# LANGUAGE TemplateHaskell, TypeFamilies #-}
module Logic.ImplicationGraph.Equivalence where

import           Control.Lens
import           Control.Monad.State
import           Control.Monad.Except
import           Control.Monad.Extra (whenM, orM, anyM)

import           Data.Data (Data)
import           Data.Maybe (mapMaybe)
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.List.Split (splitOn)
import           Data.Optic.Graph (Graph)
import qualified Data.Optic.Graph as G
import qualified Data.Optic.Graph.Extras as G
import           Data.Text.Prettyprint.Doc
import           Data.These

import qualified Logic.Type as T
import           Logic.Formula
import           Logic.Var
import           Logic.Model
import           Logic.ImplicationGraph hiding (isInductive)
import qualified Logic.Solver.Z3 as Z3

import           Text.Read (readMaybe)

type ProdGr i = Graph i (These Edge Edge) Vert

instance (Pretty a, Pretty b) => Pretty (These a b) where
  pretty = \case
    This a -> pretty "This" <+> pretty a
    That b -> pretty "That" <+> pretty b
    These a b -> pretty "These" <+> pretty a <+> pretty "|" <+> pretty b

-- | Repeatedly unwind the program until a counterexample is found or inductive
-- invariants are found.
equiv :: MonadIO m
      => Integer
      -> Integer
      -> [(Var, Var)]
      -> [(Var, Var)]
      -> ImplGr Integer
      -> ImplGr Integer
      -> m (Either Model (ProdGr Idx))
equiv e1 e2 st1 st2 g1 g2 = do
  let g' = G.mapIdxs firstInst init
  G.display "product" g'
  runSolve (loop g') >>= \case
    Left (Failed m) -> return (Left m)
    Left (Complete res) -> return (Right res)
    Right _ -> error "infinite loop terminated successfully?"
  where
    loop gr = loop =<< step' (firstInst (end+1)) gr

    init =
      equivProduct g1 g2
      & G.addVert (end+1) (QueryV
        (app2 Impl (manyAnd (map equate st1))
                   (manyAnd (map equate st2))))
      & G.addEdge end (end+1) (This $ Edge (LBool True) M.empty)

    locMerge i j = j + i * (maxJ + 1)
    maxJ = maximum (G.idxs g2)
    end = locMerge e1 e2
    equate (v1, v2) = mkEql (T.typeOf v1) (V v1) (V v2)


-- | Perform interpolation on the graph (exiting on failure), perform and induction
-- check, and unwind further if required.
step' :: Solve (These Edge Edge) m => Idx -> ProdGr Idx -> m (ProdGr Idx)
step' end g = do
  int <- interp g
  indM <- induc end int
  let isInd = M.keys $ M.filter id indM
  when (M.lookup end indM == Just True) $ throwError $ Complete int
  unwindAll (backs g) isInd end int

  where
    interp g = do
      sol <- interpolate (G.mapEdges edge g)
      case sol of
        Left e -> throwError (Failed e)
        Right g' -> do
          let vs = g' ^@.. G.iallVerts
          return $ foldr (uncurry G.addVert) g vs

    edge = \case
      This e -> e
      That e -> e
      These _ _ -> undefined

    backs = concatMap allEs . G.backEdges
    allEs ((i1, i2), e) = case e of
      These e1 e2 -> [((i1, i2), This e1), ((i1, i2), That e2)]
      e' -> [((i1, i2), e')]

induc :: MonadIO m => Idx -> ProdGr Idx -> m (Map Idx Bool)
induc = inductive' pred
  where
    pred g i = do
      let (ls, rs) = preds g i
      lInd <- allInd pred g i ls
      rInd <- allInd pred g i rs
      return [lInd, rInd]

    preds g i = mconcat $ map (\(i, e) ->
      fromThese [] [] $ bimap (const [i]) (const [i]) e) $ g ^@.. G.iedgesTo i

equivProduct :: ImplGr Integer -> ImplGr Integer -> ProdGr Integer
equivProduct g1 g2 =
  G.cartesianProductWith
    edgeMerge
    const
    locMerge
    vertMerge
    (G.mapEdges This g1)
    (G.mapEdges That g2)
  where
    edgeMerge (This e1) (That e2) = These e1 e2
    edgeMerge e1 _ = e1

    locMerge i j = j + i * (maxJ + 1)
    maxJ = maximum (G.idxs g2)

    vertMerge v1 v2 = case (v1, v2) of
      (InstanceV vs1 _, InstanceV vs2 _) -> emptyInst (vs1 ++ vs2)
      _ -> error "query in middle of equivalence graph"
