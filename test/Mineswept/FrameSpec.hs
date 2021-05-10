module Mineswept.FrameSpec (spec) where

import Test.Hspec (Spec, describe, it, pendingWith)

spec :: Spec
spec = do
  describe "makeFrame" $ do
    it "infers game status" $ do
      pendingWith "not yet implemented"
