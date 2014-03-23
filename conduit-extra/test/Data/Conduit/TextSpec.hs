{-# LANGUAGE OverloadedStrings #-}
module Data.Conduit.TextSpec (spec) where

import qualified Data.Conduit.Text as CT
import qualified Data.Conduit as C
import qualified Data.Conduit.Lift as C
import qualified Data.Conduit.List as CL
import Test.Hspec
import Test.Hspec.QuickCheck
import Data.Monoid
import Control.Monad.ST
import qualified Data.Text as T
import qualified Data.Text.Lazy.Encoding as TLE
import Data.Functor.Identity
import Control.Arrow
import Control.Applicative
import Control.Monad.Trans.Resource
import qualified Data.ByteString as S
import qualified Data.Text.Lazy as TL
import qualified Data.ByteString.Lazy as L
import Control.Monad.Trans.Resource (runExceptionT_)

spec :: Spec
spec = describe "Data.Conduit.Text" $ do
    describe "text" $ do
        let go enc tenc tdec cenc = describe enc $ do
                prop "single chunk" $ \chars -> runST $ runExceptionT_ $ do
                    let tl = TL.pack chars
                        lbs = tenc tl
                        src = CL.sourceList $ L.toChunks lbs
                    ts <- src C.$= CT.decode cenc C.$$ CL.consume
                    return $ TL.fromChunks ts == tl
                prop "many chunks" $ \chars -> runIdentity $ runExceptionT_ $ do
                    let tl = TL.pack chars
                        lbs = tenc tl
                        src = mconcat $ map (CL.sourceList . return . S.singleton) $ L.unpack lbs

                    ts <- src C.$= CT.decode cenc C.$$ CL.consume
                    return $ TL.fromChunks ts == tl

                -- Check whether raw bytes are decoded correctly, in
                -- particular that Text decoding produces an error if
                -- and only if Conduit does.
                prop "raw bytes" $ \bytes ->
                    let lbs = L.pack bytes
                        src = CL.sourceList $ L.toChunks lbs
                        etl = runException $ src C.$= CT.decode cenc C.$$ CL.consume
                        tl' = tdec lbs
                    in  case etl of
                          (Left _) -> (return $! TL.toStrict tl') `shouldThrow` anyException
                          (Right tl) -> TL.fromChunks tl `shouldBe` tl'
                prop "encoding" $ \chars -> runIdentity $ runExceptionT_ $ do
                    let tss = map T.pack chars
                        lbs = tenc $ TL.fromChunks tss
                        src = mconcat $ map (CL.sourceList . return) tss
                    bss <- src C.$= CT.encode cenc C.$$ CL.consume
                    return $ L.fromChunks bss == lbs
                prop "valid then invalid" $ \x y chars -> runIdentity $ runExceptionT_ $ do
                    let tss = map T.pack ([x, y]:chars)
                        ts = T.concat tss
                        lbs = tenc (TL.fromChunks tss) `L.append` "\0\0\0\0\0\0\0"
                        src = mapM_ C.yield $ L.toChunks lbs
                    Just x' <- src C.$$ CT.decode cenc C.=$ C.await
                    return $ x' `T.isPrefixOf` ts
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
        it "catch UTF8 exceptions, pure" $ do
            let badBS = "this is good\128\128\0that was bad"

                grabExceptions inner = do
                    res <- C.runExceptionC $ inner C.=$= CL.map Right
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

                grabExceptions inner = C.catchExceptionC
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

                grabExceptions inner = C.catchExceptionC
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

    describe "text lines" $ do
        it "works across split lines" $
            (CL.sourceList [T.pack "abc", T.pack "d\nef"] C.$= CT.lines C.$$ CL.consume) ==
                [[T.pack "abcd", T.pack "ef"]]
        it "works with multiple lines in an item" $
            (CL.sourceList [T.pack "ab\ncd\ne"] C.$= CT.lines C.$$ CL.consume) ==
                [[T.pack "ab", T.pack "cd", T.pack "e"]]
        it "works with ending on a newline" $
            (CL.sourceList [T.pack "ab\n"] C.$= CT.lines C.$$ CL.consume) ==
                [[T.pack "ab"]]
        it "works with ending a middle item on a newline" $
            (CL.sourceList [T.pack "ab\n", T.pack "cd\ne"] C.$= CT.lines C.$$ CL.consume) ==
                [[T.pack "ab", T.pack "cd", T.pack "e"]]
        it "is not too eager" $ do
            x <- CL.sourceList ["foobarbaz", error "ignore me"] C.$$ CT.decode CT.utf8 C.=$ CL.head
            x `shouldBe` Just "foobarbaz"

    describe "text lines bounded" $ do
        it "works across split lines" $
            (CL.sourceList [T.pack "abc", T.pack "d\nef"] C.$= CT.linesBounded 80 C.$$ CL.consume) ==
                [[T.pack "abcd", T.pack "ef"]]
        it "works with multiple lines in an item" $
            (CL.sourceList [T.pack "ab\ncd\ne"] C.$= CT.linesBounded 80 C.$$ CL.consume) ==
                [[T.pack "ab", T.pack "cd", T.pack "e"]]
        it "works with ending on a newline" $
            (CL.sourceList [T.pack "ab\n"] C.$= CT.linesBounded 80 C.$$ CL.consume) ==
                [[T.pack "ab"]]
        it "works with ending a middle item on a newline" $
            (CL.sourceList [T.pack "ab\n", T.pack "cd\ne"] C.$= CT.linesBounded 80 C.$$ CL.consume) ==
                [[T.pack "ab", T.pack "cd", T.pack "e"]]
        it "is not too eager" $ do
            x <- CL.sourceList ["foobarbaz", error "ignore me"] C.$$ CT.decode CT.utf8 C.=$ CL.head
            x `shouldBe` Just "foobarbaz"
        it "throws an exception when lines are too long" $ do
            x <- runExceptionT $ CL.sourceList ["hello\nworld"] C.$$ CT.linesBounded 4 C.=$ CL.consume
            show x `shouldBe` show (Left $ CT.LengthExceeded 4 :: Either CT.TextException ())
    describe "text decode" $ do
        it' "doesn't throw runtime exceptions" $ do
            let x = runIdentity $ runExceptionT $ C.yield "\x89\x243" C.$$ CT.decode CT.utf8 C.=$ CL.consume
            case x of
                Left _ -> return ()
                Right t -> error $ "This should have failed: " ++ show t

it' :: String -> IO () -> Spec
it' = it
