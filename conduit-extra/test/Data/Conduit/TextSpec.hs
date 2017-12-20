{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}
module Data.Conduit.TextSpec (spec) where

import Control.Exception (SomeException)
import qualified Data.Conduit.Text as CT
import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL
import Test.Hspec
import Test.Hspec.QuickCheck
import Data.Monoid
import Control.Monad.ST
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TEE
import qualified Data.Text.Lazy.Encoding as TLE
import Data.Functor.Identity
import Control.Arrow
import Control.Applicative
import Control.Monad.Trans.Resource
import qualified Data.ByteString as S
import qualified Data.Text.Lazy as TL
import qualified Data.ByteString.Lazy as L

spec :: Spec
spec = describe "Data.Conduit.Text" $ do
    describe "text" $ do
        let go enc tenc tdec cenc = describe enc $ do
                prop "single chunk" $ \chars -> do
                    let tl = TL.pack chars
                        lbs = tenc tl
                        src = CL.sourceList $ L.toChunks lbs
                    ts <- src C.$= CT.decode cenc C.$$ CL.consume
                    TL.fromChunks ts `shouldBe` tl
                prop "many chunks" $ \chars -> do
                    let tl = TL.pack chars
                        lbs = tenc tl
                        src = mconcat $ map (CL.sourceList . return . S.singleton) $ L.unpack lbs

                    ts <- src C.$= CT.decode cenc C.$$ CL.consume
                    TL.fromChunks ts `shouldBe` tl

                -- Check whether raw bytes are decoded correctly, in
                -- particular that Text decoding produces an error if
                -- and only if Conduit does.
                prop "raw bytes" $ \bytes -> do
                    let lbs = L.pack bytes
                        src = CL.sourceList $ L.toChunks lbs
                        tl' = tdec lbs
                        etl = src C.$= CT.decode cenc C.$$ CL.consume
                    case etl of
                          (Left _) -> (return $! TL.toStrict tl') `shouldThrow` anyException
                          (Right tl) -> TL.fromChunks tl `shouldBe` tl'
                prop "encoding" $ \chars -> do
                    let tss = map T.pack chars
                        lbs = tenc $ TL.fromChunks tss
                        src = mconcat $ map (CL.sourceList . return) tss
                    bss <- src C.$= CT.encode cenc C.$$ CL.consume
                    L.fromChunks bss `shouldBe` lbs
                prop "valid then invalid" $ \x y chars -> do
                    let tss = map T.pack ([x, y]:chars)
                        ts = T.concat tss
                        lbs = tenc (TL.fromChunks tss) `L.append` "\0\0\0\0\0\0\0"
                        src = mapM_ C.yield $ L.toChunks lbs
                    Just x' <- src C.$$ CT.decode cenc C.=$ C.await
                    (x' `T.isPrefixOf` ts) `shouldBe` True
        go "utf8" TLE.encodeUtf8 TLE.decodeUtf8 CT.utf8
        go "utf16_le" TLE.encodeUtf16LE TLE.decodeUtf16LE CT.utf16_le
        go "utf16_be" TLE.encodeUtf16BE TLE.decodeUtf16BE CT.utf16_be
        go "utf32_le" TLE.encodeUtf32LE TLE.decodeUtf32LE CT.utf32_le
        go "utf32_be" TLE.encodeUtf32BE TLE.decodeUtf32BE CT.utf32_be
        it "mixed utf16 and utf8" $ do
            let bs = "8\NUL:\NULu\NUL\215\216\217\218"
                src = C.yield bs C.$= CT.decode CT.utf16_le
            text <- src C.$$ C.await
            text `shouldBe` Just "8:u"
            (src C.$$ CL.sinkNull) `shouldThrow` anyException
        it "invalid utf8" $ do
            let bs = S.pack [0..255]
                src = C.yield bs C.$= CT.decode CT.utf8
            text <- src C.$$ C.await
            text `shouldBe` Just (T.pack $ map toEnum [0..127])
            (src C.$$ CL.sinkNull) `shouldThrow` anyException
        it "catch UTF8 exceptions" $ do
            let badBS = "this is good\128\128\0that was bad"

                grabExceptions inner = C.catchC
                    (inner C.=$= CL.map Right)
                    (\e -> C.yield (Left (e :: CT.TextException)))

            res <- C.yield badBS C.$$ (,)
                <$> (grabExceptions (CT.decode CT.utf8) C.=$ CL.consume)
                <*> CL.consume

            first (map (either (Left . show) Right)) res `shouldBe`
                ( [ Right "this is good"
                  , Left $ show $ CT.NewDecodeException "UTF-8" 12 "\128\128\0t"
                  ]
                , ["\128\128\0that was bad"]
                )
        {- FIXME uncomment
        it "catch UTF8 exceptions, pure" $ do
            let badBS = "this is good\128\128\0that was bad"

                grabExceptions inner = do
                    res <- C.runCatchC $ inner C.=$= CL.map Right
                    case res of
                        Left e -> C.yield $ Left e
                        Right () -> return ()

            let res = runIdentity $ C.yield badBS C.$$ (,)
                        <$> (grabExceptions (CT.decode CT.utf8) C.=$ CL.consume)
                        <*> CL.consume

            first (map (either (Left . show) Right)) res `shouldBe`
                ( [ Right "this is good"
                  , Left $ show $ CT.NewDecodeException "UTF-8" 12 "\128\128\0t"
                  ]
                , ["\128\128\0that was bad"]
                )
        it "catch UTF8 exceptions, catchExceptionC" $ do
            let badBS = "this is good\128\128\0that was bad"

                grabExceptions inner = C.catchCatchC
                    (inner C.=$= CL.map Right)
                    (\e -> C.yield $ Left e)

            let res = runException_ $ C.yield badBS C.$$ (,)
                        <$> (grabExceptions (CT.decode CT.utf8) C.=$ CL.consume)
                        <*> CL.consume

            first (map (either (Left . show) Right)) res `shouldBe`
                ( [ Right "this is good"
                  , Left $ show $ CT.NewDecodeException "UTF-8" 12 "\128\128\0t"
                  ]
                , ["\128\128\0that was bad"]
                )
        it "catch UTF8 exceptions, catchExceptionC, decodeUtf8" $ do
            let badBS = "this is good\128\128\0that was bad"

                grabExceptions inner = C.catchCatchC
                    (inner C.=$= CL.map Right)
                    (\e -> C.yield $ Left e)

            let res = runException_ $ C.yield badBS C.$$ (,)
                        <$> (grabExceptions CT.decodeUtf8 C.=$ CL.consume)
                        <*> CL.consume

            first (map (either (Left . show) Right)) res `shouldBe`
                ( [ Right "this is good"
                  , Left $ show $ CT.NewDecodeException "UTF-8" 12 "\128\128\0t"
                  ]
                , ["\128\128\0that was bad"]
                )
        -}
        prop "lenient UTF8 decoding" $ \good1 good2 -> do
            let bss = [TE.encodeUtf8 $ T.pack good1, "\128\129\130", TE.encodeUtf8 $ T.pack good2]
                bs = S.concat bss
                expected = TE.decodeUtf8With TEE.lenientDecode bs
                actual = runIdentity $ mapM_ C.yield bss C.$$ CT.decodeUtf8Lenient C.=$ CL.consume
            T.concat actual `shouldBe` expected

    describe "text lines" $ do
        it "yields nothing given nothing" $
            (CL.sourceList [] C.$= CT.lines C.$$ CL.consume) ==
                [[]]
        it "yields nothing given only empty text" $
            (CL.sourceList [""] C.$= CT.lines C.$$ CL.consume) ==
                [[]]
        it "works across split lines" $
            (CL.sourceList ["abc", "d\nef"] C.$= CT.lines C.$$ CL.consume) ==
                [["abcd", "ef"]]
        it "works with multiple lines in an item" $
            (CL.sourceList ["ab\ncd\ne"] C.$= CT.lines C.$$ CL.consume) ==
                [["ab", "cd", "e"]]
        it "works with ending on a newline" $
            (CL.sourceList ["ab\n"] C.$= CT.lines C.$$ CL.consume) ==
                [["ab"]]
        it "works with ending a middle item on a newline" $
            (CL.sourceList ["ab\n", "cd\ne"] C.$= CT.lines C.$$ CL.consume) ==
                [["ab", "cd", "e"]]
        it "works with empty text" $
            (CL.sourceList ["ab", "", "cd"] C.$= CT.lines C.$$ CL.consume) ==
                [["abcd"]]
        it "works with empty lines" $
            (CL.sourceList ["\n\n"] C.$= CT.lines C.$$ CL.consume) ==
                [["", ""]]

    describe "text lines bounded" $ do
        it "yields nothing given nothing" $
            (CL.sourceList [] C.$= CT.linesBounded 80 C.$$ CL.consume) ==
                [[]]
        it "yields nothing given only empty text" $
            (CL.sourceList [""] C.$= CT.linesBounded 80 C.$$ CL.consume) ==
                [[]]
        it "works across split lines" $
            (CL.sourceList ["abc", "d\nef"] C.$= CT.linesBounded 80 C.$$ CL.consume) ==
                [["abcd", "ef"]]
        it "works with multiple lines in an item" $
            (CL.sourceList ["ab\ncd\ne"] C.$= CT.linesBounded 80 C.$$ CL.consume) ==
                [["ab", "cd", "e"]]
        it "works with ending on a newline" $
            (CL.sourceList ["ab\n"] C.$= CT.linesBounded 80 C.$$ CL.consume) ==
                [["ab"]]
        it "works with ending a middle item on a newline" $
            (CL.sourceList ["ab\n", "cd\ne"] C.$= CT.linesBounded 80 C.$$ CL.consume) ==
                [["ab", "cd", "e"]]
        it "works with empty text" $
            (CL.sourceList ["ab", "", "cd"] C.$= CT.linesBounded 80 C.$$ CL.consume) ==
                [["abcd"]]
        it "works with empty lines" $
            (CL.sourceList ["\n\n"] C.$= CT.linesBounded 80 C.$$ CL.consume) ==
                [["", ""]]
        it "throws an exception when lines are too long" $ do
            let x :: Either SomeException [T.Text]
                x = CL.sourceList ["hello\nworld"] C.$$ CT.linesBounded 4 C.=$ CL.consume
            show x `shouldBe` show (Left $ CT.LengthExceeded 4 :: Either CT.TextException ())
        it "works with infinite input" $ do
            let x :: Either SomeException [T.Text]
                x = CL.sourceList (cycle ["hello"]) C.$$ CT.linesBounded 256 C.=$ CL.consume
            show x `shouldBe` show (Left $ CT.LengthExceeded 256 :: Either CT.TextException ())
    describe "text decode" $ do
        it' "doesn't throw runtime exceptions" $ do
            let x = C.yield "\x89\x243" C.$$ CT.decode CT.utf8 C.=$ CL.consume
            case x of
                Left _ -> return ()
                Right t -> error $ "This should have failed: " ++ show t
        it "is not too eager" $ do
            x <- CL.sourceList ["foobarbaz", error "ignore me"] C.$$ CT.decode CT.utf8 C.=$ CL.head
            x `shouldBe` Just "foobarbaz"

it' :: String -> IO () -> Spec
it' = it
