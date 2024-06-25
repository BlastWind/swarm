{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Interpreter for the Swarm world description DSL.
module Swarm.Game.World.Interpret (
  interpBTerm,
  interpConst,
  interpReflect,
  interpRot,
) where

import Control.Applicative (Applicative (..))
import Data.ByteString (ByteString)
import Data.Hash.Murmur (murmur3)
import Data.Tagged (unTagged)
import Numeric.Noise.Perlin (noiseValue, perlin)
import Swarm.Game.Location (pattern Location)
import Swarm.Game.World.Abstract (BTerm (..))
import Swarm.Game.World.Coords (Coords (..), coordsToLoc)
import Swarm.Game.World.Gen (Seed)
import Swarm.Game.World.Syntax (Axis (..), Rot (..))
import Swarm.Game.World.Typecheck (Const (..), Empty (..), Over (..))
import Witch (from)
import Witch.Encoding qualified as Encoding
import Prelude hiding (Applicative (..))

-- | Interpret an abstracted term into the host language.
interpBTerm :: Seed -> BTerm a -> a
interpBTerm seed (BApp f x) = interpBTerm seed f (interpBTerm seed x)
interpBTerm seed (BConst c) = interpConst seed c

-- | Interpret a constant into the host language.
interpConst :: Seed -> Const a -> a
interpConst seed = \case
  CLit a -> a
  CCell c -> c
  CIf -> \b t e -> if b then t else e
  CNot -> not
  CNeg -> negate
  CAbs -> abs
  CAnd -> (&&)
  COr -> (||)
  CAdd -> (+)
  CSub -> (-)
  CMul -> (*)
  CDiv -> (/)
  CIDiv -> div
  CMod -> mod
  CEq -> (==)
  CNeq -> (/=)
  CLt -> (<)
  CLeq -> (<=)
  CGt -> (>)
  CGeq -> (>=)
  CMask -> \b x c -> if b c then x c else empty
  CSeed -> fromIntegral seed
  CCoord ax -> \(coordsToLoc -> Location x y) -> fromIntegral (case ax of X -> x; Y -> y)
  CHash -> \(Coords ix) -> fromIntegral . murmur3 0 . unTagged . from @String @(Encoding.UTF_8 ByteString) . show $ ix
  CPerlin -> \s o k p ->
    let noise = perlin (fromIntegral s) (fromIntegral o) k p
        sample (i, j) = noiseValue noise (fromIntegral i / 2, fromIntegral j / 2, 0)
     in \(Coords ix) -> sample ix
  CReflect ax -> \w -> w . interpReflect ax
  CRot r -> \w -> w . interpRot r
  CFI -> fromInteger
  COver -> (<!>)
  K -> const
  S -> (<*>)
  I -> id
  B -> (.)
  C -> flip
  Φ -> liftA2

-- | Interprect a reflection.
interpReflect :: Axis -> Coords -> Coords
interpReflect ax (Coords (r, c)) = Coords (case ax of X -> (r, -c); Y -> (-r, c))

-- | Interpret a rotation.
interpRot :: Rot -> Coords -> Coords
interpRot rot (Coords crd) = Coords (rotTuple rot crd)
 where
  rotTuple = \case
    Rot0 -> id
    Rot90 -> \(r, c) -> (-c, r)
    Rot180 -> \(r, c) -> (-r, -c)
    Rot270 -> \(r, c) -> (c, -r)
