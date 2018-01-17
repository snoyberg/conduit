{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}
module Data.Conduit.TextSpec (spec) where

import Data.Conduit ((.|), runConduit, runConduitPure)
import Control.Exception (SomeException)
import qualified Data.Conduit.Text as CT
import qualified Data.Conduit as C
import Data.Conduit.Lift (runCatchC, catchCatchC)
import Data.Functor.Identity (runIdentity)
import qualified Data.Conduit.List as CL
import Test.Hspec
import Test.Hspec.QuickCheck
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TEE
import qualified Data.Text.Lazy.Encoding as TLE
import Control.Arrow
import qualified Data.ByteString as S
import qualified Data.Text.Lazy as TL
import qualified Data.ByteString.Lazy as L
import Control.Monad.Catch.Pure (runCatchT)

spec :: Spec
spec = describe "Data.Conduit.Text" $ do
    describe "text" $ do
        let go enc tenc tdec cenc = describe enc $ do
                prop "single chunk" $ \chars -> do
                    let tl = TL.pack chars
                        lbs = tenc tl
                        src = CL.sourceList $ L.toChunks lbs
                    ts <- runConduit $ src .| CT.decode cenc .| CL.consume
                    TL.fromChunks ts `shouldBe` tl
                prop "many chunks" $ \chars -> do
                    let tl = TL.pack chars
                        lbs = tenc tl
                        src = mconcat $ map (CL.sourceList . return . S.singleton) $ L.unpack lbs

                    ts <- runConduit $ src .| CT.decode cenc .| CL.consume
                    TL.fromChunks ts `shouldBe` tl

                -- Check whether raw bytes are decoded correctly, in
                -- particular that Text decoding produces an error if
                -- and only if Conduit does.
                prop "raw bytes" $ \bytes -> do
                    let lbs = L.pack bytes
                        src = CL.sourceList $ L.toChunks lbs
                        tl' = tdec lbs
                        etl = runConduit $ src .| CT.decode cenc .| CL.consume
                    case etl of
                          (Left _) -> (return $! TL.toStrict tl') `shouldThrow` anyException
                          (Right tl) -> TL.fromChunks tl `shouldBe` tl'
                prop "encoding" $ \chars -> do
                    let tss = map T.pack chars
                        lbs = tenc $ TL.fromChunks tss
                        src = mconcat $ map (CL.sourceList . return) tss
                    bss <- runConduit $ src .| CT.encode cenc .| CL.consume
                    L.fromChunks bss `shouldBe` lbs
                prop "valid then invalid" $ \x y chars -> do
                    let tss = map T.pack ([x, y]:chars)
                        ts = T.concat tss
                        lbs = tenc (TL.fromChunks tss) `L.append` "\0\0\0\0\0\0\0"
                        src = mapM_ C.yield $ L.toChunks lbs
                    Just x' <- runConduit $ src .| CT.decode cenc .| C.await
                    (x' `T.isPrefixOf` ts) `shouldBe` True
        go "utf8" TLE.encodeUtf8 TLE.decodeUtf8 CT.utf8
        go "utf16_le" TLE.encodeUtf16LE TLE.decodeUtf16LE CT.utf16_le
        go "utf16_be" TLE.encodeUtf16BE TLE.decodeUtf16BE CT.utf16_be
        go "utf32_le" TLE.encodeUtf32LE TLE.decodeUtf32LE CT.utf32_le
        go "utf32_be" TLE.encodeUtf32BE TLE.decodeUtf32BE CT.utf32_be
        it "mixed utf16 and utf8" $ do
            let bs = "8\NUL:\NULu\NUL\215\216\217\218"
                src = C.yield bs .| CT.decode CT.utf16_le
            text <- runConduit $ src .| C.await
            text `shouldBe` Just "8:u"
            (runConduit $ src .| CL.sinkNull) `shouldThrow` anyException
        it "invalid utf8" $ do
            let bs = S.pack [0..255]
                src = C.yield bs .| CT.decode CT.utf8
            text <- runConduit $ src .| C.await
            text `shouldBe` Just (T.pack $ map toEnum [0..127])
            (runConduit $ src .| CL.sinkNull) `shouldThrow` anyException
        it "catch UTF8 exceptions" $ do
            let badBS = "this is good\128\128\0that was bad"

                grabExceptions inner = C.catchC
                    (inner .| CL.map Right)
                    (\e -> C.yield (Left (e :: CT.TextException)))

            res <- runConduit $ C.yield badBS .| (,)
                <$> (grabExceptions (CT.decode CT.utf8) .| CL.consume)
                <*> CL.consume

            first (map (either (Left . show) Right)) res `shouldBe`
                ( [ Right "this is good"
                  , Left $ show $ CT.NewDecodeException "UTF-8" 12 "\128\128\0t"
                  ]
                , ["\128\128\0that was bad"]
                )
        it "catch UTF8 exceptions, pure" $ do
            let badBS = "this is good\128\128\0that was bad"

                grabExceptions inner = do
                    res <- runCatchC $ inner .| CL.map Right
                    case res of
                        Left e -> C.yield $ Left e
                        Right () -> return ()

            let res = runConduitPure $ C.yield badBS .| (,)
                        <$> (grabExceptions (CT.decode CT.utf8) .| CL.consume)
                        <*> CL.consume

            first (map (either (Left . show) Right)) res `shouldBe`
                ( [ Right "this is good"
                  , Left $ show $ CT.NewDecodeException "UTF-8" 12 "\128\128\0t"
                  ]
                , ["\128\128\0that was bad"]
                )
        it "catch UTF8 exceptions, catchExceptionC" $ do
            let badBS = "this is good\128\128\0that was bad"

                grabExceptions inner = catchCatchC
                    (inner .| CL.map Right)
                    (\e -> C.yield $ Left e)

            let Right res = runIdentity $ runCatchT $ runConduit $ C.yield badBS .| (,)
                        <$> (grabExceptions (CT.decode CT.utf8) .| CL.consume)
                        <*> CL.consume

            first (map (either (Left . show) Right)) res `shouldBe`
                ( [ Right "this is good"
                  , Left $ show $ CT.NewDecodeException "UTF-8" 12 "\128\128\0t"
                  ]
                , ["\128\128\0that was bad"]
                )
        it "catch UTF8 exceptions, catchExceptionC, decodeUtf8" $ do
            let badBS = ["this is good", "\128\128\0that was bad"]

                grabExceptions inner = catchCatchC
                    (inner .| CL.map Right)
                    (\e -> C.yield $ Left e)

            let Right res = runIdentity $ runCatchT $ runConduit $
                  mapM_ C.yield badBS .| (,)
                        <$> (grabExceptions CT.decodeUtf8 .| CL.consume)
                        <*> CL.consume

            first (map (either (Left . const ()) Right)) res `shouldBe`
                ( [ Right "this is good"
                  , Left ()
                  ]
                , ["\128\128\0that was bad"]
                )
        prop "lenient UTF8 decoding" $ \good1 good2 -> do
            let bss = [TE.encodeUtf8 $ T.pack good1, "\128\129\130", TE.encodeUtf8 $ T.pack good2]
                bs = S.concat bss
                expected = TE.decodeUtf8With TEE.lenientDecode bs
                actual = runConduitPure $ mapM_ C.yield bss .| CT.decodeUtf8Lenient .| CL.consume
            T.concat actual `shouldBe` expected

    describe "text lines" $ do
        it "yields nothing given nothing" $
            (runConduit $ CL.sourceList [] .| CT.lines .| CL.consume) ==
                [[]]
        it "yields nothing given only empty text" $
            (runConduit $ CL.sourceList [""] .| CT.lines .| CL.consume) ==
                [[]]
        it "works across split lines" $
            (runConduit $ CL.sourceList ["abc", "d\nef"] .| CT.lines .| CL.consume) ==
                [["abcd", "ef"]]
        it "works with multiple lines in an item" $
            (runConduit $ CL.sourceList ["ab\ncd\ne"] .| CT.lines .| CL.consume) ==
                [["ab", "cd", "e"]]
        it "works with ending on a newline" $
            (runConduit $ CL.sourceList ["ab\n"] .| CT.lines .| CL.consume) ==
                [["ab"]]
        it "works with ending a middle item on a newline" $
            (runConduit $ CL.sourceList ["ab\n", "cd\ne"] .| CT.lines .| CL.consume) ==
                [["ab", "cd", "e"]]
        it "works with empty text" $
            (runConduit $ CL.sourceList ["ab", "", "cd"] .| CT.lines .| CL.consume) ==
                [["abcd"]]
        it "works with empty lines" $
            (runConduit $ CL.sourceList ["\n\n"] .| CT.lines .| CL.consume) ==
                [["", ""]]

    describe "text lines bounded" $ do
        it "yields nothing given nothing" $
            (runConduit $ CL.sourceList [] .| CT.linesBounded 80 .| CL.consume) ==
                [[]]
        it "yields nothing given only empty text" $
            (runConduit $ CL.sourceList [""] .| CT.linesBounded 80 .| CL.consume) ==
                [[]]
        it "works across split lines" $
            (runConduit $ CL.sourceList ["abc", "d\nef"] .| CT.linesBounded 80 .| CL.consume) ==
                [["abcd", "ef"]]
        it "works with multiple lines in an item" $
            (runConduit $ CL.sourceList ["ab\ncd\ne"] .| CT.linesBounded 80 .| CL.consume) ==
                [["ab", "cd", "e"]]
        it "works with ending on a newline" $
            (runConduit $ CL.sourceList ["ab\n"] .| CT.linesBounded 80 .| CL.consume) `shouldBe`
                [["ab"]]
        it "works with ending a middle item on a newline" $
            (runConduit $ CL.sourceList ["ab\n", "cd\ne"] .| CT.linesBounded 80 .| CL.consume) `shouldBe`
                [["ab", "cd", "e"]]
        it "works with empty text" $
            (runConduit $ CL.sourceList ["ab", "", "cd"] .| CT.linesBounded 80 .| CL.consume) `shouldBe`
                [["abcd"]]
        it "works with empty lines" $
            (runConduit (CL.sourceList ["\n\n"] .| CT.linesBounded 80 .| CL.consume)) `shouldBe`
                [["", ""]]
        it "throws an exception when lines are too long" $ do
            let x :: Either SomeException [T.Text]
                x = runConduit $ CL.sourceList ["hello\nworld"] .| CT.linesBounded 4 .| CL.consume
            show x `shouldBe` show (Left $ CT.LengthExceeded 4 :: Either CT.TextException ())
        it "works with infinite input" $ do
            let x :: Either SomeException [T.Text]
                x = runConduit $ CL.sourceList (cycle ["hello"]) .| CT.linesBounded 256 .| CL.consume
            show x `shouldBe` show (Left $ CT.LengthExceeded 256 :: Either CT.TextException ())
    describe "text decode" $ do
        it' "doesn't throw runtime exceptions" $ do
            let x = runConduit $ C.yield "\x89\x243" .| CT.decode CT.utf8 .| CL.consume
            case x of
                Left _ -> return ()
                Right t -> error $ "This should have failed: " ++ show t
        it "is not too eager" $ do
            x <- runConduit $ CL.sourceList ["foobarbaz", error "ignore me"] .| CT.decode CT.utf8 .| CL.head
            x `shouldBe` Just "foobarbaz"

it' :: String -> IO () -> Spec
it' = it
