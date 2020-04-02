module Generators.Basic where

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

natural :: Gen Natural
natural = Gen.integral (Range.exponentialFrom 0 0 18446744073709552000)
