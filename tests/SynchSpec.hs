{-# LANGUAGE Arrows #-}

module SynchSpec (spec) where

import Control.Arrow (returnA)
import Protolude
import Synch (SF, action, counter, runSF)
import Synch.RefStore
import Synch.System (execSystem)
import Test.Hspec

spec :: Spec
spec = do
    describe "ArrowChoice instance" $ do
        it "only executes one of the branches at a time" $ do
            r1 <- newRef []
            r2 <- newRef []

            let put1 c = void $ modifyRef r1 (c :)
            let put2 c = void $ modifyRef r2 (c :)

            execSystem 10 $
                runSF $
                    pingPong put1 put2

            list1 <- reverse <$> getRef r1
            list2 <- reverse <$> getRef r2

            list1 `shouldBe` [4, 5, 6, 7, 12, 13, 14, 15]
            list2 `shouldBe` [0, 1, 2, 3, 8, 9, 10, 11, 16, 17, 18, 19]

pingPong :: (Int -> IO ()) -> (Int -> IO ()) -> SF IO () Bool
pingPong put1 put2 = proc () -> do
    c <- counter -< True

    if (c `mod` 8) > 3
        then action put1 -< c
        else action put2 -< c

    let oneMoreStep = c < 19
    returnA -< oneMoreStep
