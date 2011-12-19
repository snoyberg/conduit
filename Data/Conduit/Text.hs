{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
-- |
-- Module: Data.Enumerator.Text
-- Copyright: 2010-2011 John Millikin
-- License: MIT
--
-- Maintainer: jmillikin@gmail.com
-- Portability: portable
--
-- Character-oriented alternatives to "Data.Enumerator.List". Note that the
-- enumeratees in this module must unpack their inputs to work properly. If
-- you do not need to handle leftover input on a char-by-char basis, the
-- chunk-oriented versions will be much faster.
--
-- This module is intended to be imported qualified:
--
-- @
-- import qualified Data.Enumerator.Text as ET
-- @
--
-- Since: 0.2
module Data.Conduit.Text
    (

{-
    -- * IO
      sourceHandle
    , enumFile
    , iterHandle

    -- * List analogues

    -- ** Folds
    , fold
    , foldM

    -- ** Maps
    , Data.Enumerator.Text.map
    , Data.Enumerator.Text.mapM
    , Data.Enumerator.Text.mapM_
    , Data.Enumerator.Text.concatMap
    , concatMapM

    -- ** Accumulating maps
    , mapAccum
    , mapAccumM
    , concatMapAccum
    , concatMapAccumM

    -- ** Infinite streams
    , Data.Enumerator.Text.iterate
    , iterateM
    , Data.Enumerator.Text.repeat
    , repeatM

    -- ** Bounded streams
    , Data.Enumerator.Text.replicate
    , replicateM
    , generateM
    , unfold
    , unfoldM

    -- ** Dropping input
    , Data.Enumerator.Text.drop
    , Data.Enumerator.Text.dropWhile
    , Data.Enumerator.Text.filter
    , filterM

    -- ** Consumers
    , Data.Enumerator.Text.head
    , head_
    , Data.Enumerator.Text.take
    , takeWhile
    , consume

    -- ** Zipping
    , zip
    , zip3
    , zip4
    , zip5
    , zip6
    , zip7
    , zipWith
    , zipWith3
    , zipWith4
    , zipWith5
    , zipWith6
    , zipWith7

    -- ** Unsorted
    , require
    , isolate
    , isolateWhile
    , splitWhen
    , lines
-}

    -- * Text codecs
      Codec
    , encode
    , decode
    , utf8
    , utf16_le
    , utf16_be
    , utf32_le
    , utf32_be
    , ascii
    , iso8859_1

    ) where

import qualified Prelude
import           Prelude hiding (head, drop, takeWhile, lines, zip, zip3, zipWith, zipWith3)

import           Control.Arrow (first)
import qualified Control.Exception as Exc
import qualified Control.Monad as CM
import           Control.Monad (liftM)
import           Control.Monad.IO.Class (MonadIO)
import           Control.Monad.Trans.Class (lift)
import           Data.Bits ((.&.), (.|.), shiftL)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import           Data.Char (ord)
import           Data.Maybe (catMaybes)
import           Data.Monoid (mappend)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import qualified Data.Text.Lazy as TL
import           Data.Word (Word8, Word16)
import qualified System.IO as IO
import           System.IO.Error (isEOFError)
import           System.IO.Unsafe (unsafePerformIO)
import           Numeric (showIntAtBase)
import           Data.Char (toUpper, intToDigit)
import           Data.Typeable (Typeable)

import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL
import Control.Monad.Trans.Resource (ResourceThrow (..))

{-
-- | Consume the entire input stream with a strict left fold, one character
-- at a time.
--
-- Since: 0.4.8
fold :: Monad m => (b -> Char -> b) -> b
     -> Iteratee T.Text m b
fold step = EL.fold (T.foldl' step)

-- | Consume the entire input stream with a strict monadic left fold, one
-- character at a time.
--
-- Since: 0.4.8
foldM :: Monad m => (b -> Char -> m b) -> b
      -> Iteratee T.Text m b
foldM step = EL.foldM (\b txt -> CM.foldM step b (T.unpack txt))

-- | Enumerates a stream of characters by repeatedly applying a function to
-- some state.
--
-- Similar to 'Data.Enumerator.Text.iterate'.
--
-- Since: 0.4.8
unfold :: Monad m => (s -> Maybe (Char, s)) -> s -> Enumerator T.Text m b
unfold f = checkContinue1 $ \loop s k -> case f s of
    Nothing -> continue k
    Just (c, s') -> k (Chunks [T.singleton c]) >>== loop s'

-- | Enumerates a stream of characters by repeatedly applying a computation
-- to some state.
--
-- Similar to 'iterateM'.
--
-- Since: 0.4.8
unfoldM :: Monad m => (s -> m (Maybe (Char, s))) -> s -> Enumerator T.Text m b
unfoldM f = checkContinue1 $ \loop s k -> do
    fs <- lift (f s)
    case fs of
        Nothing -> continue k
        Just (c, s') -> k (Chunks [T.singleton c]) >>== loop s'

-- | @'Data.Enumerator.Text.map' f@ applies /f/ to each input character and
-- feeds the resulting outputs to the inner iteratee.
--
-- Since: 0.4.8
map :: Monad m => (Char -> Char) -> Enumeratee T.Text T.Text m b
map f = Data.Enumerator.Text.concatMap (\x -> T.singleton (f x))

-- | @'Data.Enumerator.Text.mapM' f@ applies /f/ to each input character
-- and feeds the resulting outputs to the inner iteratee.
--
-- Since: 0.4.8
mapM :: Monad m => (Char -> m Char) -> Enumeratee T.Text T.Text m b
mapM f = Data.Enumerator.Text.concatMapM (\x -> liftM T.singleton (f x))

-- | @'Data.Enumerator.Text.mapM_' f@ applies /f/ to each input character,
-- and discards the results.
--
-- Since: 0.4.11
mapM_ :: Monad m => (Char -> m ()) -> Iteratee T.Text m ()
mapM_ f = foldM (\_ x -> f x >> return ()) ()

-- | @'Data.Enumerator.Text.concatMap' f@ applies /f/ to each input
-- character and feeds the resulting outputs to the inner iteratee.
--
-- Since: 0.4.8
concatMap :: Monad m => (Char -> T.Text) -> Enumeratee T.Text T.Text m b
concatMap f = Data.Enumerator.Text.concatMapM (return . f)

-- | @'concatMapM' f@ applies /f/ to each input character and feeds the
-- resulting outputs to the inner iteratee.
--
-- Since: 0.4.8
concatMapM :: Monad m => (Char -> m T.Text) -> Enumeratee T.Text T.Text m b
concatMapM f = checkDone (continue . step) where
    step k EOF = yield (Continue k) EOF
    step k (Chunks xs) = loop k (TL.unpack (TL.fromChunks xs))

    loop k [] = continue (step k)
    loop k (x:xs) = do
        fx <- lift (f x)
        k (Chunks [fx]) >>==
            checkDoneEx (Chunks [T.pack xs]) (`loop` xs)

-- | Similar to 'Data.Enumerator.Text.concatMap', but with a stateful step
-- function.
--
-- Since: 0.4.11
concatMapAccum :: Monad m => (s -> Char -> (s, T.Text)) -> s -> Enumeratee T.Text T.Text m b
concatMapAccum f s0 = checkDone (continue . step s0) where
    step _ k EOF = yield (Continue k) EOF
    step s k (Chunks xs) = loop s k xs

    loop s k [] = continue (step s k)
    loop s k (x:xs) = case T.uncons x of
        Nothing -> loop s k xs
        Just (c, x') -> case f s c of
            (s', ai) -> k (Chunks [ai]) >>==
                checkDoneEx (Chunks (x':xs)) (\k' -> loop s' k' (x':xs))

-- | Similar to 'concatMapM', but with a stateful step function.
--
-- Since: 0.4.11
concatMapAccumM :: Monad m => (s -> Char -> m (s, T.Text)) -> s -> Enumeratee T.Text T.Text m b
concatMapAccumM f s0 = checkDone (continue . step s0) where
    step _ k EOF = yield (Continue k) EOF
    step s k (Chunks xs) = loop s k xs

    loop s k [] = continue (step s k)
    loop s k (x:xs) = case T.uncons x of
        Nothing -> loop s k xs
        Just (c, x') -> do
            (s', ai) <- lift (f s c)
            k (Chunks [ai]) >>==
                checkDoneEx (Chunks (x':xs)) (\k' -> loop s' k' (x':xs))

-- | Similar to 'Data.Enumerator.Text.map', but with a stateful step
-- function.
--
-- Since: 0.4.9
mapAccum :: Monad m => (s -> Char -> (s, Char)) -> s -> Enumeratee T.Text T.Text m b
mapAccum f = concatMapAccum (\s c -> case f s c of (s', c') -> (s', T.singleton c'))

-- | Similar to 'Data.Enumerator.Text.mapM', but with a stateful step
-- function.
--
-- Since: 0.4.9
mapAccumM :: Monad m => (s -> Char -> m (s, Char)) -> s -> Enumeratee T.Text T.Text m b
mapAccumM f = concatMapAccumM (\s c -> do
    (s', c') <- f s c
    return (s', T.singleton c'))

-- | @'Data.Enumerator.Text.iterate' f x@ enumerates an infinite stream of
-- repeated applications of /f/ to /x/.
--
-- Analogous to 'Prelude.iterate'.
--
-- Since: 0.4.8
iterate :: Monad m => (Char -> Char) -> Char -> Enumerator T.Text m b
iterate f = checkContinue1 $ \loop s k -> k (Chunks [T.singleton s]) >>== loop (f s)

-- | Similar to 'Data.Enumerator.Text.iterate', except the iteration
-- function is monadic.
--
-- Since: 0.4.8
iterateM :: Monad m => (Char -> m Char) -> Char -> Enumerator T.Text m b
iterateM f base = worker (return base) where
    worker = checkContinue1 $ \loop m_char k -> do
        char <- lift m_char
        k (Chunks [T.singleton char]) >>== loop (f char)

-- | Enumerates an infinite stream of a single character.
--
-- Analogous to 'Prelude.repeat'.
--
-- Since: 0.4.8
repeat :: Monad m => Char -> Enumerator T.Text m b
repeat char = EL.repeat (T.singleton char)

-- | Enumerates an infinite stream of characters. Each character is computed
-- by the underlying monad.
--
-- Since: 0.4.8
repeatM :: Monad m => m Char -> Enumerator T.Text m b
repeatM next = EL.repeatM (liftM T.singleton next)

-- | @'Data.Enumerator.Text.replicate' n x@ enumerates a stream containing
-- /n/ copies of /x/.
--
-- Since: 0.4.8
replicate :: Monad m => Integer -> Char -> Enumerator T.Text m b
replicate n byte = EL.replicate n (T.singleton byte)

-- | @'replicateM' n m_x@ enumerates a stream of /n/ characters, with each
-- character computed by /m_x/.
--
-- Since: 0.4.8
replicateM :: Monad m => Integer -> m Char -> Enumerator T.Text m b
replicateM n next = EL.replicateM n (liftM T.singleton next)

-- | Like 'repeatM', except the computation may terminate the stream by
-- returning 'Nothing'.
--
-- Since: 0.4.8
generateM :: Monad m => m (Maybe Char) -> Enumerator T.Text m b
generateM next = EL.generateM (liftM (liftM T.singleton) next)

-- | Applies a predicate to the stream. The inner iteratee only receives
-- characters for which the predicate is @True@.
--
-- Since: 0.4.8
filter :: Monad m => (Char -> Bool) -> Enumeratee T.Text T.Text m b
filter p = Data.Enumerator.Text.concatMap (\x -> T.pack [x | p x])

-- | Applies a monadic predicate to the stream. The inner iteratee only
-- receives characters for which the predicate returns @True@.
--
-- Since: 0.4.8
filterM :: Monad m => (Char -> m Bool) -> Enumeratee T.Text T.Text m b
filterM p = Data.Enumerator.Text.concatMapM (\x -> liftM T.pack (CM.filterM p [x]))

-- | @'Data.Enumerator.Text.take' n@ extracts the next /n/ characters from
-- the stream, as a lazy Text.
--
-- Since: 0.4.5
take :: Monad m => Integer -> Iteratee T.Text m TL.Text
take n | n <= 0 = return TL.empty
take n = continue (loop id n) where
    loop acc n' (Chunks xs) = iter where
        lazy = TL.fromChunks xs
        len = toInteger (TL.length lazy)

        iter = if len < n'
            then continue (loop (acc . TL.append lazy) (n' - len))
            else let
                (xs', extra) = TL.splitAt (fromInteger n') lazy
                in yield (acc xs') (toChunks extra)
    loop acc _ EOF = yield (acc TL.empty) EOF

-- | @'takeWhile' p@ extracts input from the stream until the first character
-- which does not match the predicate.
--
-- Since: 0.4.5
takeWhile :: Monad m => (Char -> Bool) -> Iteratee T.Text m TL.Text
takeWhile p = continue (loop id) where
    loop acc (Chunks []) = continue (loop acc)
    loop acc (Chunks xs) = iter where
        lazy = TL.fromChunks xs
        (xs', extra) = tlSpanBy p lazy
        iter = if TL.null extra
            then continue (loop (acc . TL.append lazy))
            else yield (acc xs') (toChunks extra)
    loop acc EOF = yield (acc TL.empty) EOF

-- | @'consume' = 'takeWhile' (const True)@
--
-- Since: 0.4.5
consume :: Monad m => Iteratee T.Text m TL.Text
consume = continue (loop id) where
    loop acc (Chunks []) = continue (loop acc)
    loop acc (Chunks xs) = iter where
        lazy = TL.fromChunks xs
        iter = continue (loop (acc . TL.append lazy))
    loop acc EOF = yield (acc TL.empty) EOF

-- | Pass input from a stream through two iteratees at once. Excess input is
-- yielded if it was not consumed by either iteratee.
--
-- Analogous to 'Data.List.zip'.
--
-- Since: 0.4.14
zip :: Monad m
    => Iteratee T.Text m b1
    -> Iteratee T.Text m b2
    -> Iteratee T.Text m (b1, b2)
zip i1 i2 = continue step where
    step (Chunks []) = continue step
    step stream@(Chunks _) = do
        let enumStream s = case s of
            Continue k -> k stream
            Yield b extra -> yield b (mappend extra stream)
            Error err -> throwError err

        s1 <- lift (runIteratee (enumStream ==<< i1))
        s2 <- lift (runIteratee (enumStream ==<< i2))

        case (s1, s2) of
            (Continue k1, Continue k2) -> zip (continue k1) (continue k2)
            (Yield b1 _, Continue k2) -> zip (yield b1 (Chunks [])) (continue k2)
            (Continue k1, Yield b2 _) -> zip (continue k1) (yield b2 (Chunks []))
            (Yield b1 ex1, Yield b2 ex2) -> yield (b1, b2) (shorter ex1 ex2)
            (Error err, _) -> throwError err
            (_, Error err) -> throwError err

    step EOF = do
        b1 <- enumEOF =<< lift (runIteratee i1)
        b2 <- enumEOF =<< lift (runIteratee i2)
        return (b1, b2)

    shorter c1@(Chunks xs) c2@(Chunks ys) = let
        xs' = T.concat xs
        ys' = T.concat ys
        in if T.length xs' < T.length ys'
            then c1
            else c2
    shorter _ _ = EOF

-- | Pass input from a stream through three iteratees at once. Excess input is
-- yielded if it was not consumed by any iteratee.
--
-- Analogous to 'Data.List.zip3'.
--
-- Since: 0.4.14
zip3 :: Monad m
     => Iteratee T.Text m b1
     -> Iteratee T.Text m b2
     -> Iteratee T.Text m b3
     -> Iteratee T.Text m (b1, b2, b3)
zip3 i1 i2 i3 = do
    (b1, (b2, b3)) <- zip i1 (zip i2 i3)
    return (b1, b2, b3)
{-# INLINE zip3 #-}

-- | Pass input from a stream through four iteratees at once. Excess input is
-- yielded if it was not consumed by any iteratee.
--
-- Analogous to 'Data.List.zip4'.
--
-- Since: 0.4.14
zip4 :: Monad m
     => Iteratee T.Text m b1
     -> Iteratee T.Text m b2
     -> Iteratee T.Text m b3
     -> Iteratee T.Text m b4
     -> Iteratee T.Text m (b1, b2, b3, b4)
zip4 i1 i2 i3 i4 = do
    (b1, (b2, b3, b4)) <- zip i1 (zip3 i2 i3 i4)
    return (b1, b2, b3, b4)
{-# INLINE zip4 #-}

-- | Pass input from a stream through five iteratees at once. Excess input is
-- yielded if it was not consumed by any iteratee.
--
-- Analogous to 'Data.List.zip5'.
--
-- Since: 0.4.14
zip5 :: Monad m
     => Iteratee T.Text m b1
     -> Iteratee T.Text m b2
     -> Iteratee T.Text m b3
     -> Iteratee T.Text m b4
     -> Iteratee T.Text m b5
     -> Iteratee T.Text m (b1, b2, b3, b4, b5)
zip5 i1 i2 i3 i4 i5 = do
    (b1, (b2, b3, b4, b5)) <- zip i1 (zip4 i2 i3 i4 i5)
    return (b1, b2, b3, b4, b5)
{-# INLINE zip5 #-}

-- | Pass input from a stream through six iteratees at once. Excess input is
-- yielded if it was not consumed by any iteratee.
--
-- Analogous to 'Data.List.zip6'.
--
-- Since: 0.4.14
zip6 :: Monad m
     => Iteratee T.Text m b1
     -> Iteratee T.Text m b2
     -> Iteratee T.Text m b3
     -> Iteratee T.Text m b4
     -> Iteratee T.Text m b5
     -> Iteratee T.Text m b6
     -> Iteratee T.Text m (b1, b2, b3, b4, b5, b6)
zip6 i1 i2 i3 i4 i5 i6 = do
    (b1, (b2, b3, b4, b5, b6)) <- zip i1 (zip5 i2 i3 i4 i5 i6)
    return (b1, b2, b3, b4, b5, b6)
{-# INLINE zip6 #-}

-- | Pass input from a stream through seven iteratees at once. Excess input is
-- yielded if it was not consumed by any iteratee.
--
-- Analogous to 'Data.List.zip7'.
--
-- Since: 0.4.14
zip7 :: Monad m
     => Iteratee T.Text m b1
     -> Iteratee T.Text m b2
     -> Iteratee T.Text m b3
     -> Iteratee T.Text m b4
     -> Iteratee T.Text m b5
     -> Iteratee T.Text m b6
     -> Iteratee T.Text m b7
     -> Iteratee T.Text m (b1, b2, b3, b4, b5, b6, b7)
zip7 i1 i2 i3 i4 i5 i6 i7 = do
    (b1, (b2, b3, b4, b5, b6, b7)) <- zip i1 (zip6 i2 i3 i4 i5 i6 i7)
    return (b1, b2, b3, b4, b5, b6, b7)
{-# INLINE zip7 #-}

-- | Pass input from a stream through two iteratees at once. Excess input is
-- yielded if it was not consumed by either iteratee. Output from the
-- iteratees is combined with a user-provided function.
--
-- Analogous to 'Data.List.zipWith'.
--
-- Since: 0.4.14
zipWith :: Monad m
        => (b1 -> b2 -> c)
        -> Iteratee T.Text m b1
        -> Iteratee T.Text m b2
        -> Iteratee T.Text m c
zipWith f i1 i2 = do
    (b1, b2) <- zip i1 i2
    return (f b1 b2)
{-# INLINE zipWith #-}

-- | Pass input from a stream through two iteratees at once. Excess input is
-- yielded if it was not consumed by either iteratee. Output from the
-- iteratees is combined with a user-provided function.
--
-- Analogous to 'Data.List.zipWith3'.
--
-- Since: 0.4.14
zipWith3 :: Monad m
         => (b1 -> b2 -> b3 -> c)
         -> Iteratee T.Text m b1
         -> Iteratee T.Text m b2
         -> Iteratee T.Text m b3
         -> Iteratee T.Text m c
zipWith3 f i1 i2 i3 = do
    (b1, b2, b3) <- zip3 i1 i2 i3
    return (f b1 b2 b3)
{-# INLINE zipWith3 #-}

-- | Pass input from a stream through two iteratees at once. Excess input is
-- yielded if it was not consumed by either iteratee. Output from the
-- iteratees is combined with a user-provided function.
--
-- Analogous to 'Data.List.zipWith4'.
--
-- Since: 0.4.14
zipWith4 :: Monad m
         => (b1 -> b2 -> b3 -> b4 -> c)
         -> Iteratee T.Text m b1
         -> Iteratee T.Text m b2
         -> Iteratee T.Text m b3
         -> Iteratee T.Text m b4
         -> Iteratee T.Text m c
zipWith4 f i1 i2 i3 i4 = do
    (b1, b2, b3, b4) <- zip4 i1 i2 i3 i4
    return (f b1 b2 b3 b4)
{-# INLINE zipWith4 #-}

-- | Pass input from a stream through two iteratees at once. Excess input is
-- yielded if it was not consumed by either iteratee. Output from the
-- iteratees is combined with a user-provided function.
--
-- Analogous to 'Data.List.zipWith5'.
--
-- Since: 0.4.14
zipWith5 :: Monad m
         => (b1 -> b2 -> b3 -> b4 -> b5 -> c)
         -> Iteratee T.Text m b1
         -> Iteratee T.Text m b2
         -> Iteratee T.Text m b3
         -> Iteratee T.Text m b4
         -> Iteratee T.Text m b5
         -> Iteratee T.Text m c
zipWith5 f i1 i2 i3 i4 i5 = do
    (b1, b2, b3, b4, b5) <- zip5 i1 i2 i3 i4 i5
    return (f b1 b2 b3 b4 b5)
{-# INLINE zipWith5 #-}

-- | Pass input from a stream through two iteratees at once. Excess input is
-- yielded if it was not consumed by either iteratee. Output from the
-- iteratees is combined with a user-provided function.
--
-- Analogous to 'Data.List.zipWith6'.
--
-- Since: 0.4.14
zipWith6 :: Monad m
         => (b1 -> b2 -> b3 -> b4 -> b5 -> b6 -> c)
         -> Iteratee T.Text m b1
         -> Iteratee T.Text m b2
         -> Iteratee T.Text m b3
         -> Iteratee T.Text m b4
         -> Iteratee T.Text m b5
         -> Iteratee T.Text m b6
         -> Iteratee T.Text m c
zipWith6 f i1 i2 i3 i4 i5 i6 = do
    (b1, b2, b3, b4, b5, b6) <- zip6 i1 i2 i3 i4 i5 i6
    return (f b1 b2 b3 b4 b5 b6)
{-# INLINE zipWith6 #-}

-- | Pass input from a stream through two iteratees at once. Excess input is
-- yielded if it was not consumed by either iteratee. Output from the
-- iteratees is combined with a user-provided function.
--
-- Analogous to 'Data.List.zipWith7'.
--
-- Since: 0.4.14
zipWith7 :: Monad m
         => (b1 -> b2 -> b3 -> b4 -> b5 -> b6 -> b7 -> c)
         -> Iteratee T.Text m b1
         -> Iteratee T.Text m b2
         -> Iteratee T.Text m b3
         -> Iteratee T.Text m b4
         -> Iteratee T.Text m b5
         -> Iteratee T.Text m b6
         -> Iteratee T.Text m b7
         -> Iteratee T.Text m c
zipWith7 f i1 i2 i3 i4 i5 i6 i7 = do
    (b1, b2, b3, b4, b5, b6, b7) <- zip7 i1 i2 i3 i4 i5 i6 i7
    return (f b1 b2 b3 b4 b5 b6 b7)
{-# INLINE zipWith7 #-}

-- | Get the next character from the stream, or 'Nothing' if the stream has
-- ended.
--
-- Since: 0.4.5
head :: Monad m => Iteratee T.Text m (Maybe Char)
head = continue loop where
    loop (Chunks xs) = case TL.uncons (TL.fromChunks xs) of
        Just (char, extra) -> yield (Just char) (toChunks extra)
        Nothing -> head
    loop EOF = yield Nothing EOF

-- | Get the next element from the stream, or raise an error if the stream
-- has ended.
--
-- Since: 0.4.14
head_ :: Monad m => Iteratee T.Text m Char
head_ = head >>= \x -> case x of
    Just x' -> return x'
    Nothing -> throwError (Exc.ErrorCall "head_: stream has ended")

-- | @'drop' n@ ignores /n/ characters of input from the stream.
--
-- Since: 0.4.5
drop :: Monad m => Integer -> Iteratee T.Text m ()
drop n | n <= 0 = return ()
drop n = continue (loop n) where
    loop n' (Chunks xs) = iter where
        lazy = TL.fromChunks xs
        len = toInteger (TL.length lazy)
        iter = if len < n'
            then drop (n' - len)
            else yield () (toChunks (TL.drop (fromInteger n') lazy))
    loop _ EOF = yield () EOF

-- | @'Data.Enumerator.Text.dropWhile' p@ ignores input from the stream
-- until the first character which does not match the predicate.
--
-- Since: 0.4.5
dropWhile :: Monad m => (Char -> Bool) -> Iteratee T.Text m ()
dropWhile p = continue loop where
    loop (Chunks xs) = iter where
        lazy = TL.dropWhile p (TL.fromChunks xs)
        iter = if TL.null lazy
            then continue loop
            else yield () (toChunks lazy)
    loop EOF = yield () EOF

-- | @'require' n@ buffers input until at least /n/ characters are available,
-- or throws an error if the stream ends early.
--
-- Since: 0.4.5
require :: Monad m => Integer -> Iteratee T.Text m ()
require n | n <= 0 = return ()
require n = continue (loop id n) where
    loop acc n' (Chunks xs) = iter where
        lazy = TL.fromChunks xs
        len = toInteger (TL.length lazy)
        iter = if len < n'
            then continue (loop (acc . TL.append lazy) (n' - len))
            else yield () (toChunks (acc lazy))
    loop _ _ EOF = throwError (Exc.ErrorCall "require: Unexpected EOF")

-- | @'isolate' n@ reads at most /n/ characters from the stream, and passes
-- them to its iteratee. If the iteratee finishes early, characters continue
-- to be consumed from the outer stream until /n/ have been consumed.
--
-- Since: 0.4.5
isolate :: Monad m => Integer -> Enumeratee T.Text T.Text m b
isolate n step | n <= 0 = return step
isolate n (Continue k) = continue loop where
    loop (Chunks []) = continue loop
    loop (Chunks xs) = iter where
        lazy = TL.fromChunks xs
        len = toInteger (TL.length lazy)

        iter = if len <= n
            then k (Chunks xs) >>== isolate (n - len)
            else let
                (s1, s2) = TL.splitAt (fromInteger n) lazy
                in k (toChunks s1) >>== (`yield` toChunks s2)
    loop EOF = k EOF >>== (`yield` EOF)
isolate n step = drop n >> return step

-- | @'isolateWhile' p@ reads characters from the stream until /p/ is false, and
-- passes them to its iteratee. If the iteratee finishes early, characters
-- continue to be consumed from the outer stream until /p/ is false.
--
-- Since: 0.4.16
isolateWhile :: Monad m => (Char -> Bool) -> Enumeratee T.Text T.Text m b
isolateWhile p (Continue k) = continue loop where
    loop (Chunks []) = continue loop
    loop (Chunks xs) = iter where
        lazy = TL.fromChunks xs
        (s1, s2) = TL.span p lazy
        iter = if TL.null s2
            then k (Chunks xs) >>== isolateWhile p
            else k (toChunks s1) >>== (`yield` toChunks s2)
    loop EOF = k EOF >>== (`yield` EOF)
isolateWhile p step = Data.Enumerator.Text.dropWhile p >> return step

-- | Split on characters satisfying a given predicate.
--
-- Since: 0.4.8
splitWhen :: Monad m => (Char -> Bool) -> Enumeratee T.Text T.Text m b
splitWhen p = loop where
    loop = checkDone step
    step k = isEOF >>= \eof -> if eof
        then yield (Continue k) EOF
        else do
            lazy <- takeWhile (not . p)
            let text = textToStrict lazy
            eof <- isEOF
            drop 1
            if TL.null lazy && eof
                then yield (Continue k) EOF
                else k (Chunks [text]) >>== loop

-- | @'lines' = 'splitWhen' (== '\n')@
--
-- Since: 0.4.8
lines :: Monad m => Enumeratee T.Text T.Text m b
lines = splitWhen (== '\n')

-- | Read lines of text from the handle, and stream them to an 'Iteratee'.
-- If an exception occurs during file IO, enumeration will stop and 'Error'
-- will be returned. Exceptions from the iteratee are not caught.
--
-- The handle should be opened with an appropriate text encoding, and
-- in 'IO.ReadMode' or 'IO.ReadWriteMode'.
--
-- Since: 0.2
enumHandle :: MonadIO m => IO.Handle
           -> Enumerator T.Text m b
enumHandle h = checkContinue0 $ \loop k -> do
    let getText = Exc.catch
        (Just `fmap` TIO.hGetLine h)
        (\err -> if isEOFError err
            then return Nothing
            else Exc.throwIO err)

    maybeText <- tryIO getText
    case maybeText of
        Nothing -> continue k
        Just text -> k (Chunks [text]) >>== loop



-- | Opens a file path in text mode, and passes the handle to 'enumHandle'.
-- The file will be closed when the 'Iteratee' finishes.
--
-- Since: 0.2
enumFile :: FilePath -> Enumerator T.Text IO b
enumFile path step = do
    h <- tryIO (IO.openFile path IO.ReadMode)
    Iteratee $ Exc.finally
        (runIteratee (enumHandle h step))
        (IO.hClose h)


-- | Read text from a stream and write it to a handle. If an exception
-- occurs during file IO, enumeration will stop and 'Error' will be
-- returned.
--
-- The handle should be opened with an appropriate text encoding, and
-- in 'IO.WriteMode' or 'IO.ReadWriteMode'.
--
-- Since: 0.2
iterHandle :: MonadIO m => IO.Handle
           -> Iteratee T.Text m ()
iterHandle h = continue step where
    step EOF = yield () EOF
    step (Chunks []) = continue step
    step (Chunks chunks) = do
        tryIO (CM.mapM_ (TIO.hPutStr h) chunks)
        continue step
-}

data Codec = Codec
    { codecName :: T.Text
    , codecEncode
        :: T.Text
        -> (B.ByteString, Maybe (TextException, T.Text))
    , codecDecode
        :: B.ByteString
        -> (T.Text, Either
            (TextException, B.ByteString)
            B.ByteString)
    }

instance Show Codec where
    showsPrec d c = showParen (d > 10) $
        showString "Codec " . shows (codecName c)

-- | Convert text into bytes, using the provided codec. If the codec is
-- not capable of representing an input character, an error will be thrown.
--
-- Since: 0.2
encode :: ResourceThrow m => Codec -> C.ConduitM T.Text m B.ByteString
encode codec = CL.mapM $ \t -> do
    let (bs, mexc) = codecEncode codec t
    maybe (return bs) (resourceThrow . fst) mexc


-- | Convert bytes into text, using the provided codec. If the codec is
-- not capable of decoding an input byte sequence, an error will be thrown.
--
-- Since: 0.2
decode :: ResourceThrow m => Codec -> C.ConduitM B.ByteString m T.Text
decode codec = C.conduitMState
    Nothing
    push
    close
  where
    push mb input = do
        (mb', ts) <- go' mb input
        return $ (mb', C.ConduitResult C.StreamOpen [] ts)
    close mb input = do
        (mb', ts) <- go' mb input
        case mb' of
            Nothing -> return $ C.ConduitCloseResult [] ts
            Just b
                | B.null b -> error "Data.Conduit.Text.decode: Received a null chunk"
                | otherwise -> lift $ resourceThrow $ DecodeException codec (B.head b)

    go' mb input = do
        let bss = maybe id (:) mb input
        either (lift . resourceThrow) return $ go bss id

    go [] front = Right (Nothing, front [])
    go (x:xs) front
        | B.null x = go xs front
    go (x:xs) front =
        case extra of
            Left (exc, _) -> Left exc
            Right bs
                | B.null bs -> go xs front'
                | otherwise ->
                    case xs of
                        y:ys -> go (B.append bs y:ys) front'
                        [] -> Right (Just bs, front' [])
      where
        (text, extra) = codecDecode codec x
        front' = front . (text:)

data TextException = DecodeException Codec Word8
                   | EncodeException Codec Char
    deriving (Show, Typeable)
instance Exc.Exception TextException

byteSplits :: B.ByteString
           -> [(B.ByteString, B.ByteString)]
byteSplits bytes = loop (B.length bytes) where
    loop 0 = [(B.empty, bytes)]
    loop n = B.splitAt n bytes : loop (n - 1)

splitSlowly :: (B.ByteString -> T.Text)
            -> B.ByteString
            -> (T.Text, Either
                (TextException, B.ByteString)
                B.ByteString)
splitSlowly dec bytes = valid where
    valid = firstValid (Prelude.map decFirst splits)
    splits = byteSplits bytes
    firstValid = Prelude.head . catMaybes
    tryDec = tryEvaluate . dec

    decFirst (a, b) = case tryDec a of
        Left _ -> Nothing
        Right text -> Just (text, case tryDec b of
            Left exc -> Left (exc, b)

            -- this case shouldn't occur, since splitSlowly
            -- is only called when parsing failed somewhere
            Right _ -> Right B.empty)

utf8 :: Codec
utf8 = Codec name enc dec where
    name = T.pack "UTF-8"
    enc text = (TE.encodeUtf8 text, Nothing)
    dec bytes = case splitQuickly bytes of
        Just (text, extra) -> (text, Right extra)
        Nothing -> splitSlowly TE.decodeUtf8 bytes

    splitQuickly bytes = loop 0 >>= maybeDecode where
        required x0
            | x0 .&. 0x80 == 0x00 = 1
            | x0 .&. 0xE0 == 0xC0 = 2
            | x0 .&. 0xF0 == 0xE0 = 3
            | x0 .&. 0xF8 == 0xF0 = 4

            -- Invalid input; let Text figure it out
            | otherwise           = 0

        maxN = B.length bytes

        loop n | n == maxN = Just (TE.decodeUtf8 bytes, B.empty)
        loop n = let
            req = required (B.index bytes n)
            tooLong = first TE.decodeUtf8 (B.splitAt n bytes)
            decodeMore = loop $! n + req
            in if req == 0
                then Nothing
                else if n + req > maxN
                    then Just tooLong
                    else decodeMore

utf16_le :: Codec
utf16_le = Codec name enc dec where
    name = T.pack "UTF-16-LE"
    enc text = (TE.encodeUtf16LE text, Nothing)
    dec bytes = case splitQuickly bytes of
        Just (text, extra) -> (text, Right extra)
        Nothing -> splitSlowly TE.decodeUtf16LE bytes

    splitQuickly bytes = maybeDecode (loop 0) where
        maxN = B.length bytes

        loop n |  n      == maxN = decodeAll
               | (n + 1) == maxN = decodeTo n
        loop n = let
            req = utf16Required
                (B.index bytes n)
                (B.index bytes (n + 1))
            decodeMore = loop $! n + req
            in if n + req > maxN
                then decodeTo n
                else decodeMore

        decodeTo n = first TE.decodeUtf16LE (B.splitAt n bytes)
        decodeAll = (TE.decodeUtf16LE bytes, B.empty)

utf16_be :: Codec
utf16_be = Codec name enc dec where
    name = T.pack "UTF-16-BE"
    enc text = (TE.encodeUtf16BE text, Nothing)
    dec bytes = case splitQuickly bytes of
        Just (text, extra) -> (text, Right extra)
        Nothing -> splitSlowly TE.decodeUtf16BE bytes

    splitQuickly bytes = maybeDecode (loop 0) where
        maxN = B.length bytes

        loop n |  n      == maxN = decodeAll
               | (n + 1) == maxN = decodeTo n
        loop n = let
            req = utf16Required
                (B.index bytes (n + 1))
                (B.index bytes n)
            decodeMore = loop $! n + req
            in if n + req > maxN
                then decodeTo n
                else decodeMore

        decodeTo n = first TE.decodeUtf16BE (B.splitAt n bytes)
        decodeAll = (TE.decodeUtf16BE bytes, B.empty)

utf16Required :: Word8 -> Word8 -> Int
utf16Required x0 x1 = required where
    required = if x >= 0xD800 && x <= 0xDBFF
        then 4
        else 2
    x :: Word16
    x = (fromIntegral x1 `shiftL` 8) .|. fromIntegral x0

utf32_le :: Codec
utf32_le = Codec name enc dec where
    name = T.pack "UTF-32-LE"
    enc text = (TE.encodeUtf32LE text, Nothing)
    dec bs = case utf32SplitBytes TE.decodeUtf32LE bs of
        Just (text, extra) -> (text, Right extra)
        Nothing -> splitSlowly TE.decodeUtf32LE bs

utf32_be :: Codec
utf32_be = Codec name enc dec where
    name = T.pack "UTF-32-BE"
    enc text = (TE.encodeUtf32BE text, Nothing)
    dec bs = case utf32SplitBytes TE.decodeUtf32BE bs of
        Just (text, extra) -> (text, Right extra)
        Nothing -> splitSlowly TE.decodeUtf32BE bs

utf32SplitBytes :: (B.ByteString -> T.Text)
                -> B.ByteString
                -> Maybe (T.Text, B.ByteString)
utf32SplitBytes dec bytes = split where
    split = maybeDecode (dec toDecode, extra)
    len = B.length bytes
    lenExtra = mod len 4

    lenToDecode = len - lenExtra
    (toDecode, extra) = if lenExtra == 0
        then (bytes, B.empty)
        else B.splitAt lenToDecode bytes

ascii :: Codec
ascii = Codec name enc dec where
    name = T.pack "ASCII"
    enc text = (bytes, extra) where
        (safe, unsafe) = T.span (\c -> ord c <= 0x7F) text
        bytes = B8.pack (T.unpack safe)
        extra = if T.null unsafe
            then Nothing
            else Just (EncodeException ascii (T.head unsafe), unsafe)

    dec bytes = (text, extra) where
        (safe, unsafe) = B.span (<= 0x7F) bytes
        text = T.pack (B8.unpack safe)
        extra = if B.null unsafe
            then Right B.empty
            else Left (DecodeException ascii (B.head unsafe), unsafe)

iso8859_1 :: Codec
iso8859_1 = Codec name enc dec where
    name = T.pack "ISO-8859-1"
    enc text = (bytes, extra) where
        (safe, unsafe) = T.span (\c -> ord c <= 0xFF) text
        bytes = B8.pack (T.unpack safe)
        extra = if T.null unsafe
            then Nothing
            else Just (EncodeException iso8859_1 (T.head unsafe), unsafe)

    dec bytes = (T.pack (B8.unpack bytes), Right B.empty)

tryEvaluate :: a -> Either TextException a
tryEvaluate = unsafePerformIO . Exc.try . Exc.evaluate

maybeDecode:: (a, b) -> Maybe (a, b)
maybeDecode (a, b) = case tryEvaluate a of
    Left _ -> Nothing
    Right _ -> Just (a, b)

reprChar :: Char -> String
reprChar c = "U+" ++ pad0 4 (showIntAtBase 16 (toUpper . intToDigit) (ord c) "")

reprWord :: Word8 -> String
reprWord w = "0x" ++ pad0 2 (showIntAtBase 16 (toUpper . intToDigit) w "")

pad0 :: Int -> String -> String
pad0 size str = padded where
    len = Prelude.length str
    padded = if len >= size
        then str
        else Prelude.replicate (size - len) '0' ++ str
