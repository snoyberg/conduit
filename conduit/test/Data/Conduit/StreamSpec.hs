{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE CPP #-}
module Data.Conduit.StreamSpec where

import           Control.Applicative
import qualified Control.Monad
import           Control.Monad (MonadPlus(..), liftM)
import           Control.Monad.Identity (Identity, runIdentity)
import           Control.Monad.State (StateT(..), get, put)
import           Data.Conduit
import           Data.Conduit.Internal.Fusion
import           Data.Conduit.Internal.List.Stream
import           Data.Conduit.List
import qualified Data.Foldable as F
import           Data.Function (on)
import qualified Data.List
import qualified Data.Maybe
import           Data.Monoid (Monoid(..))
import           Prelude
    ((.), ($), (>>=), (=<<), return, (==), Int, id, Maybe(..), Monad,
     Eq, Show, String, Functor, fst, snd)
import qualified Prelude
import qualified Safe
import           Test.Hspec
import           Test.QuickCheck

spec :: Spec
spec = describe "Comparing list function to" $ do
    qit "unfold" $
        \(getBlind -> f, initial :: Int) ->
            unfold f initial `checkInfiniteProducer`
            (Data.List.unfoldr f initial :: [Int])
    qit "unfoldS" $
        \(getBlind -> f, initial :: Int) ->
            unfoldS f initial `checkInfiniteStreamProducer`
            (Data.List.unfoldr f initial :: [Int])
    qit "unfoldM" $
        \(getBlind -> f, initial :: Int) ->
            unfoldM f initial `checkInfiniteProducerM`
            (unfoldrM f initial :: M [Int])
    qit "unfoldMS" $
        \(getBlind -> f, initial :: Int) ->
            unfoldMS f initial `checkInfiniteStreamProducerM`
            (unfoldrM f initial :: M [Int])
    qit "sourceList" $
        \(xs :: [Int]) ->
            sourceList xs `checkProducer` xs
    qit "sourceListS" $
        \(xs :: [Int]) ->
            sourceListS xs `checkStreamProducer` xs
    qit "enumFromTo" $
        \(fr :: Small Int, to :: Small Int) ->
            enumFromTo fr to `checkProducer`
            Prelude.enumFromTo fr to
    qit "enumFromToS" $
        \(fr :: Small Int, to :: Small Int) ->
            enumFromToS fr to `checkStreamProducer`
            Prelude.enumFromTo fr to
    qit "enumFromToS_int" $
        \(getSmall -> fr :: Int, getSmall -> to :: Int) ->
            enumFromToS_int fr to `checkStreamProducer`
            Prelude.enumFromTo fr to
    qit "iterate" $
        \(getBlind -> f, initial :: Int) ->
            iterate f initial `checkInfiniteProducer`
            Prelude.iterate f initial
    qit "iterateS" $
        \(getBlind -> f, initial :: Int) ->
            iterateS f initial `checkInfiniteStreamProducer`
            Prelude.iterate f initial
    qit "replicate" $
        \(getSmall -> n, getSmall -> x) ->
            replicate n x `checkProducer`
            (Prelude.replicate n x :: [Int])
    qit "replicateS" $
        \(getSmall -> n, getSmall -> x) ->
            replicateS n x `checkStreamProducer`
            (Prelude.replicate n x :: [Int])
    qit "replicateM" $
        \(getSmall -> n, getBlind -> f) ->
            replicateM n f `checkProducerM`
            (Control.Monad.replicateM n f :: M [Int])
    qit "replicateMS" $
        \(getSmall -> n, getBlind -> f) ->
            replicateMS n f `checkStreamProducerM`
            (Control.Monad.replicateM n f :: M [Int])
    qit "fold" $
        \(getBlind -> f, initial :: Int) ->
            fold f initial `checkConsumer`
            Data.List.foldl' f initial
    qit "foldS" $
        \(getBlind -> f, initial :: Int) ->
            foldS f initial `checkStreamConsumer`
            Data.List.foldl' f initial
    qit "foldM" $
        \(getBlind -> f, initial :: Int) ->
            foldM f initial `checkConsumerM`
            (Control.Monad.foldM f initial :: [Int] -> M Int)
    qit "foldMS" $
        \(getBlind -> f, initial :: Int) ->
            foldMS f initial `checkStreamConsumerM`
            (Control.Monad.foldM f initial :: [Int] -> M Int)
    qit "foldMap" $
        \(getBlind -> (f :: Int -> Sum Int)) ->
            foldMap f `checkConsumer`
            F.foldMap f
    qit "mapM_" $
        \(getBlind -> (f :: Int -> M ())) ->
            mapM_ f `checkConsumerM`
            Prelude.mapM_ f
    qit "mapM_S" $
        \(getBlind -> (f :: Int -> M ())) ->
            mapM_S f `checkStreamConsumerM`
            Prelude.mapM_ f
    qit "take" $
        \(getSmall -> n) ->
            take n `checkConsumer`
            Prelude.take n
    qit "takeS" $
        \(getSmall -> n) ->
            takeS n `checkStreamConsumer`
            Prelude.take n
    qit "head" $
        \() ->
            head `checkConsumer`
            Safe.headMay
    qit "headS" $
        \() ->
            headS `checkStreamConsumer`
            Safe.headMay
    qit "peek" $
        \() ->
            peek `checkConsumer`
            Safe.headMay
    qit "map" $
        \(getBlind -> (f :: Int -> Int)) ->
            map f `checkConduit`
            Prelude.map f
    qit "mapS" $
        \(getBlind -> (f :: Int -> Int)) ->
            mapS f `checkStreamConduit`
            Prelude.map f
    qit "mapM" $
        \(getBlind -> (f :: Int -> M Int)) ->
            mapM f `checkConduitM`
            Prelude.mapM f
    qit "mapMS" $
        \(getBlind -> (f :: Int -> M Int)) ->
            mapMS f `checkStreamConduitM`
            Prelude.mapM f
    qit "iterM" $
        \(getBlind -> (f :: Int -> M ())) ->
            iterM f `checkConduitM`
            iterML f
    qit "iterMS" $
        \(getBlind -> (f :: Int -> M ())) ->
            iterMS f `checkStreamConduitM`
            iterML f
    qit "mapMaybe" $
        \(getBlind -> (f :: Int -> Maybe Int)) ->
            mapMaybe f `checkConduit`
            Data.Maybe.mapMaybe f
    qit "mapMaybeS" $
        \(getBlind -> (f :: Int -> Maybe Int)) ->
            mapMaybeS f `checkStreamConduit`
            Data.Maybe.mapMaybe f
    qit "mapMaybeM" $
        \(getBlind -> (f :: Int -> M (Maybe Int))) ->
            mapMaybeM f `checkConduitM`
            mapMaybeML f
    qit "mapMaybeMS" $
        \(getBlind -> (f :: Int -> M (Maybe Int))) ->
            mapMaybeMS f `checkStreamConduitM`
            mapMaybeML f
    qit "catMaybes" $
        \() ->
            catMaybes `checkConduit`
            (Data.Maybe.catMaybes :: [Maybe Int] -> [Int])
    qit "catMaybesS" $
        \() ->
            catMaybesS `checkStreamConduit`
            (Data.Maybe.catMaybes :: [Maybe Int] -> [Int])
    qit "concat" $
        \() ->
            concat `checkConduit`
            (Prelude.concat :: [[Int]] -> [Int])
    qit "concatS" $
        \() ->
            concatS `checkStreamConduit`
            (Prelude.concat :: [[Int]] -> [Int])
    qit "concatMap" $
        \(getBlind -> f) ->
            concatMap f `checkConduit`
            (Prelude.concatMap f :: [Int] -> [Int])
    qit "concatMapS" $
        \(getBlind -> f) ->
            concatMapS f `checkStreamConduit`
            (Prelude.concatMap f :: [Int] -> [Int])
    qit "concatMapM" $
        \(getBlind -> (f :: Int -> M [Int])) ->
            concatMapM f `checkConduitM`
            concatMapML f
    qit "concatMapMS" $
        \(getBlind -> (f :: Int -> M [Int])) ->
            concatMapMS f `checkStreamConduitM`
            concatMapML f
    qit "concatMapAccum" $
        \(getBlind -> (f :: Int -> Int -> (Int, [Int])), initial :: Int) ->
            concatMapAccum f initial `checkConduit`
            concatMapAccumL f initial
    qit "concatMapAccumS" $
        \(getBlind -> (f :: Int -> Int -> (Int, [Int])), initial :: Int) ->
            concatMapAccumS f initial `checkStreamConduit`
            concatMapAccumL f initial
    {-qit "mapAccum" $
        \(getBlind -> (f :: Int -> Int -> (Int, [Int])), initial :: Int) ->
            mapAccum f initial `checkConduitResult`
            mapAccumL f initial-}
    qit "mapAccumS" $
        \(getBlind -> (f :: Int -> Int -> (Int, [Int])), initial :: Int) ->
            mapAccumS f initial `checkStreamConduitResult`
            mapAccumL f initial
    {-qit "mapAccumM" $
        \(getBlind -> (f :: Int -> Int -> M (Int, [Int])), initial :: Int) ->
            mapAccumM f initial `checkConduitResultM`
            mapAccumML f initial-}
    qit "mapAccumMS" $
        \(getBlind -> (f :: Int -> Int -> M (Int, [Int])), initial :: Int) ->
            mapAccumMS f initial `checkStreamConduitResultM`
            mapAccumML f initial
    {-qit "scan" $
        \(getBlind -> (f :: Int -> Int -> Int), initial :: Int) ->
            scan f initial `checkConduitResult`
            scanL f initial-}
    {-qit "scanM" $
        \(getBlind -> (f :: Int -> Int -> M Int), initial :: Int) ->
            scanM f initial `checkConduitResultM`
            scanML f initial-}
    qit "mapFoldable" $
        \(getBlind -> (f :: Int -> [Int])) ->
            mapFoldable f `checkConduit`
            mapFoldableL f
    qit "mapFoldableS" $
        \(getBlind -> (f :: Int -> [Int])) ->
            mapFoldableS f `checkStreamConduit`
            mapFoldableL f
    qit "mapFoldableM" $
        \(getBlind -> (f :: Int -> M [Int])) ->
            mapFoldableM f `checkConduitM`
            mapFoldableML f
    qit "mapFoldableMS" $
        \(getBlind -> (f :: Int -> M [Int])) ->
            mapFoldableMS f `checkStreamConduitM`
            mapFoldableML f
    qit "consume" $
        \() ->
            consume `checkConsumer`
            id
    qit "consumeS" $
        \() ->
            consumeS `checkStreamConsumer`
            id
    qit "groupBy" $
        \(getBlind -> f) ->
            groupBy f `checkConduit`
            (Data.List.groupBy f :: [Int] -> [[Int]])
    qit "groupByS" $
        \(getBlind -> f) ->
            groupByS f `checkStreamConduit`
            (Data.List.groupBy f :: [Int] -> [[Int]])
    qit "groupOn1" $
        \(getBlind -> (f :: Int -> Int)) ->
            groupOn1 f `checkConduit`
            groupOn1L f
    qit "groupOn1S" $
        \(getBlind -> (f :: Int -> Int)) ->
            groupOn1S f `checkStreamConduit`
            groupOn1L f
    qit "isolate" $
        \n ->
            isolate n `checkConduit`
            (Data.List.take n :: [Int] -> [Int])
    qit "isolateS" $
        \n ->
            isolateS n `checkStreamConduit`
            (Data.List.take n :: [Int] -> [Int])
    qit "filter" $
        \(getBlind -> f) ->
            filter f `checkConduit`
            (Data.List.filter f :: [Int] -> [Int])
    qit "filterS" $
        \(getBlind -> f) ->
            filterS f `checkStreamConduit`
            (Data.List.filter f :: [Int] -> [Int])
    qit "sourceNull" $
        \() ->
            sourceNull `checkProducer`
            ([] :: [Int])
    qit "sourceNullS" $
        \() ->
            sourceNullS `checkStreamProducer`
            ([] :: [Int])

qit :: (Arbitrary a, Testable prop, Show a)
    => String -> (a -> prop) -> Spec
qit n f = it n $ property $ forAll arbitrary f

--------------------------------------------------------------------------------
-- Quickcheck utilities for pure conduits / streams

checkProducer :: (Show a, Eq a) => Source Identity a -> [a] -> Property
checkProducer c l  = checkProducerM' runIdentity c (return l)

checkStreamProducer :: (Show a, Eq a) => StreamSource Identity a -> [a] -> Property
checkStreamProducer s l = checkStreamProducerM' runIdentity s (return l)

checkInfiniteProducer :: (Show a, Eq a) => Source Identity a -> [a] -> Property
checkInfiniteProducer c l = checkInfiniteProducerM' runIdentity c (return l)

checkInfiniteStreamProducer :: (Show a, Eq a) => StreamSource Identity a -> [a] -> Property
checkInfiniteStreamProducer s l = checkInfiniteStreamProducerM' runIdentity s (return l)

checkConsumer :: (Show b, Eq b) => Consumer Int Identity b -> ([Int] -> b) -> Property
checkConsumer c l = checkConsumerM' runIdentity c (return . l)

checkStreamConsumer :: (Show b, Eq b) => StreamConsumer Int Identity b -> ([Int] -> b) -> Property
checkStreamConsumer c l = checkStreamConsumerM' runIdentity c (return . l)

checkConduit :: (Show a, Arbitrary a, Show b, Eq b) => Conduit a Identity b -> ([a] -> [b]) -> Property
checkConduit c l = checkConduitM' runIdentity c (return . l)

checkStreamConduit :: (Show a, Arbitrary a, Show b, Eq b) => StreamConduit a Identity b -> ([a] -> [b]) -> Property
checkStreamConduit c l = checkStreamConduitM' runIdentity c (return . l)

-- checkConduitResult :: (Show a, Arbitrary a, Show b, Eq b, Show r, Eq r) => ConduitM a b Identity r -> ([a] -> ([b], r)) -> Property
-- checkConduitResult c l = checkConduitResultM' runIdentity c (return . l)

checkStreamConduitResult :: (Show a, Arbitrary a, Show b, Eq b, Show r, Eq r) => StreamConduitM a b Identity r -> ([a] -> ([b], r)) -> Property
checkStreamConduitResult c l = checkStreamConduitResultM' runIdentity c (return . l)

--------------------------------------------------------------------------------
-- Quickcheck utilities for conduits / streams in the M monad.

checkProducerM :: (Show a, Eq a) => Source M a -> M [a] -> Property
checkProducerM = checkProducerM' runM

checkStreamProducerM :: (Show a, Eq a) => StreamSource M a -> M [a] -> Property
checkStreamProducerM = checkStreamProducerM' runM

checkInfiniteProducerM :: (Show a, Eq a) => Source M a -> M [a] -> Property
checkInfiniteProducerM = checkInfiniteProducerM' (fst . runM)

checkInfiniteStreamProducerM :: (Show a, Eq a) => StreamSource M a -> M [a] -> Property
checkInfiniteStreamProducerM = checkInfiniteStreamProducerM' (fst . runM)

checkConsumerM :: (Show b, Eq b) => Consumer Int M b -> ([Int] -> M b) -> Property
checkConsumerM  = checkConsumerM' runM

checkStreamConsumerM :: (Show b, Eq b) => StreamConsumer Int M b -> ([Int] -> M b) -> Property
checkStreamConsumerM  = checkStreamConsumerM' runM

checkConduitM :: (Show a, Arbitrary a, Show b, Eq b) => Conduit a M b -> ([a] -> M [b]) -> Property
checkConduitM = checkConduitM' runM

checkStreamConduitM :: (Show a, Arbitrary a, Show b, Eq b) => StreamConduit a M b -> ([a] -> M [b]) -> Property
checkStreamConduitM = checkStreamConduitM' runM

-- checkConduitResultM :: (Show a, Arbitrary a, Show b, Eq b, Show r, Eq r) => ConduitM a b M r -> ([a] -> M ([b], r)) -> Property
-- checkConduitResultM = checkConduitResultM' runM

checkStreamConduitResultM :: (Show a, Arbitrary a, Show b, Eq b, Show r, Eq r) => StreamConduitM a b M r -> ([a] -> M ([b], r)) -> Property
checkStreamConduitResultM = checkStreamConduitResultM' runM

--------------------------------------------------------------------------------
-- Quickcheck utilities for monadic streams / conduits
-- These are polymorphic in which Monad is used.

checkProducerM' :: (Show a, Monad m, Show b, Eq b)
                => (m [a] -> b)
                -> Source m a
                -> m [a]
                -> Property
checkProducerM' f c l =
    f (preventFusion c $$ consume)
    ===
    f l

checkStreamProducerM' :: (Show a, Monad m, Show b, Eq b)
                      => (m [a] -> b)
                      -> StreamSource m a
                      -> m [a]
                      -> Property
checkStreamProducerM' f s l =
    f (liftM fst $ evalStream $ s emptyStream)
    ===
    f l

checkInfiniteProducerM' :: (Show a, Monad m, Show b, Eq b)
                        => (m [a] -> b)
                        -> Source m a
                        -> m [a]
                        -> Property
checkInfiniteProducerM' f s l =
    checkProducerM' f
        (preventFusion s $= isolate 10)
        (liftM (Prelude.take 10) l)

checkInfiniteStreamProducerM' :: (Show a, Monad m, Show b, Eq b)
                              => (m [a] -> b)
                              -> StreamSource m a
                              -> m [a]
                              -> Property
checkInfiniteStreamProducerM' f s l =
    f (liftM snd $ evalStream $ takeS 10 $ s emptyStream)
    ===
    f (liftM (Prelude.take 10) l)

checkConsumerM' :: (Show a, Monad m, Show b, Eq b)
                => (m a -> b)
                -> Consumer Int m a
                -> ([Int] -> m a)
                -> Property
checkConsumerM' f c l = forAll arbitrary $ \xs ->
    f (sourceList xs $$ preventFusion c)
    ===
    f (l xs)

checkStreamConsumerM' :: (Show a, Monad m, Show b, Eq b)
                      => (m a -> b)
                      -> StreamConsumer Int m a
                      -> ([Int] -> m a)
                      -> Property
checkStreamConsumerM' f s l = forAll arbitrary $ \xs ->
    f (liftM snd $ evalStream $ s $ sourceListS xs emptyStream)
    ===
    f (l xs)

checkConduitM' :: (Show a, Arbitrary a, Monad m, Show c, Eq c)
               => (m [b] -> c)
               -> Conduit a m b
               -> ([a] -> m [b])
               -> Property
checkConduitM' f c l = forAll arbitrary $ \xs ->
    f (sourceList xs $= preventFusion c $$ consume)
    ===
    f (l xs)

checkStreamConduitM' :: (Show a, Arbitrary a, Monad m, Show c, Eq c)
                     =>  (m [b] -> c)
                     -> StreamConduit a m b
                     -> ([a] -> m [b])
                     -> Property
checkStreamConduitM' f s l = forAll arbitrary $ \xs ->
    f (liftM fst $ evalStream $ s $ sourceListS xs emptyStream)
    ===
    f (l xs)

-- TODO: Fixing this would allow comparing conduit consumers against
-- their list versions.
--
-- checkConduitResultM' :: (Show a, Arbitrary a, Monad m, Show c, Eq c)
--                      => (m ([b], r) -> c)
--                      -> ConduitM a b m r
--                      -> ([a] -> m ([b], r))
--                      -> Property
-- checkConduitResultM' f c l = FIXME forAll arbitrary $ \xs ->
--     f (sourceList xs $= preventFusion c $$ consume)
--     ===
--     f (l xs)

checkStreamConduitResultM' :: (Show a, Arbitrary a, Monad m, Show c, Eq c)
                           =>  (m ([b], r) -> c)
                           -> StreamConduitM a b m r
                           -> ([a] -> m ([b], r))
                           -> Property
checkStreamConduitResultM' f s l = forAll arbitrary $ \xs ->
    f (evalStream $ s $ sourceListS xs emptyStream)
    ===
    f (l xs)

emptyStream :: Monad m => Stream m () ()
emptyStream = Stream (\_ -> return $ Stop ()) (return ())

evalStream :: Monad m => Stream m o r -> m ([o], r)
evalStream (Stream step s0) = go =<< s0
  where
    go s = do
        res <- step s
        case res of
            Stop r -> return ([], r)
            Skip s' -> go s'
            Emit s' x -> liftM (\(l, r) -> (x:l, r)) (go s')

--------------------------------------------------------------------------------
-- Misc utilities

-- Prefer this to creating an orphan instance for Data.Monoid.Sum:

newtype Sum a = Sum a
  deriving (Eq, Show, Arbitrary)

instance Prelude.Num a => Monoid (Sum a) where
  mempty = Sum 0
  mappend (Sum x) (Sum y) = Sum $ x Prelude.+ y

preventFusion :: a -> a
preventFusion = id
{-# INLINE [0] preventFusion #-}

newtype M a = M (StateT Int Identity a)
  deriving (Functor, Applicative, Monad)

instance Arbitrary a => Arbitrary (M a) where
    arbitrary = do
        f <- arbitrary
        return $ do
            s <- M get
            let (x, s') = f s
            M (put s')
            return x

runM :: M a -> (a, Int)
runM (M m) = runIdentity $ runStateT m 0

--------------------------------------------------------------------------------
-- List versions of some functions

iterML :: Monad m => (a -> m ()) -> [a] -> m [a]
iterML f = Prelude.mapM (\a -> f a >>= \() -> return a)

mapMaybeML :: Monad m => (a -> m (Maybe b)) -> [a] -> m [b]
mapMaybeML f = liftM Data.Maybe.catMaybes . Prelude.mapM f

concatMapML :: Monad m => (a -> m [b]) -> [a] -> m [b]
concatMapML f = liftM Prelude.concat . Prelude.mapM f

concatMapAccumL :: (a -> s -> (s, [b])) -> s -> [a] -> [b]
concatMapAccumL f acc0 =
    runIdentity . concatMapAccumML (\a acc -> return $ f a acc) acc0

mapAccumL :: (a -> s -> (s, b)) -> s -> [a] -> ([b], s)
mapAccumL f acc0 =
    runIdentity . mapAccumML (\a acc -> return $ f a acc) acc0

concatMapAccumML :: Monad m => (a -> s -> m (s, [b])) -> s -> [a] -> m [b]
concatMapAccumML f acc0 =
    liftM (Prelude.concat . fst) . mapAccumML f acc0

scanL :: (a -> b -> b) -> b -> [a] -> ([b], b)
scanL f = mapAccumL (\a b -> let r = f a b in (r, r))

scanML :: Monad m => (a -> b -> m b) -> b -> [a] -> m ([b], b)
scanML f = mapAccumML (\a b -> f a b >>= \r -> return (r, r))

mapFoldableL :: F.Foldable f => (a -> f b) -> [a] -> [b]
mapFoldableL f = runIdentity . mapFoldableML (return . f)

mapFoldableML :: (Monad m, F.Foldable f) => (a -> m (f b)) -> [a] -> m [b]
mapFoldableML f = concatMapML (liftM F.toList . f)

groupOn1L :: Eq b => (a -> b) -> [a] -> [(a, [a])]
groupOn1L f =
    Data.List.map (\(x:xs) -> (x, xs)) . Data.List.groupBy ((==) `on` f)

mapAccumML :: Monad m => (a -> s -> m (s, b)) -> s -> [a] -> m ([b], s)
mapAccumML f s0 = go s0
  where
    go s [] = return ([], s)
    go s (x:xs) = do
        (s', r) <- f x s
        liftM (\(l, o) -> (r:l, o)) $ go s' xs

--------------------------------------------------------------------------------
-- Utilities taken from monad-loops package

-- http://hackage.haskell.org/package/monad-loops

-- |See 'Data.List.unfoldr'.  This is a monad-friendly version of that.
unfoldrM :: (Monad m) => (a -> m (Maybe (b,a))) -> a -> m [b]
unfoldrM = unfoldrM'

-- |See 'Data.List.unfoldr'.  This is a monad-friendly version of that, with a
-- twist.  Rather than returning a list, it returns any MonadPlus type of your
-- choice.
unfoldrM' :: (Monad m, MonadPlus f) => (a -> m (Maybe (b,a))) -> a -> m (f b)
unfoldrM' f = go
    where go z = do
            x <- f z
            case x of
                Nothing         -> return mzero
                Just (x', z')   -> do
                        xs <- go z'
                        return (return x' `mplus` xs)
