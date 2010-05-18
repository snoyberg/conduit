-- | Some helpers for parsing data out of a raw WAI 'Request'.

module Network.Wai.Parse
    ( parseQueryString
    ) where

import Network.Wai
import qualified Data.ByteString as S
import Data.Word (Word8)
import Data.Bits

type BufferedBS = ([Word8], S.ByteString)

uncons :: BufferedBS -> Maybe (Word8, BufferedBS)
uncons ((w:ws), s) = Just (w, (ws, s))
uncons ([], s)
    | S.null s = Nothing
    | otherwise = Just (S.head s, ([], S.tail s))

cons :: Word8 -> BufferedBS -> BufferedBS
cons w (ws, s) = (w:ws, s)

breakDiscard :: Word8 -> S.ByteString -> (S.ByteString, S.ByteString)
breakDiscard w s =
    let (x, y) = S.break (== w) s
     in (x, S.drop 1 y)

-- | Split out the query string into a list of keys and values. A few
-- importants points:
--
-- * There is no way to distinguish between a parameter with no value and a
-- parameter with an empty value. Eg, "foo=" and "foo" are the same.
--
-- * The result returned is still bytestrings, since we perform no character
-- decoding here. Most likely, you will want to use UTF-8 decoding, but this is
-- left to the user of the library.
--
-- * Percent decoding errors are ignored. In particular, "%Q" will be output as
-- "%Q".
parseQueryString :: S.ByteString -> [(S.ByteString, S.ByteString)]
parseQueryString q | S.null q = []
parseQueryString q =
    let (x, xs) = breakDiscard wand q
     in parsePair x : parseQueryString xs
  where
    parsePair x =
        let (k, v) = breakDiscard wequal x
         in (decode k, decode v)
    decode x = fst $ S.unfoldrN (S.length x) go (NoPercent, ([], x))
    go (state, x) =
        case (state, uncons x) of
            (NoPercent, Nothing) -> Nothing
            (NoChar, Nothing) -> Just (wpercent, (NoPercent, x))
            (OneChar (w, _), Nothing) ->
                Just (wpercent, (NoPercent, cons w x))
            (NoPercent, Just (w, ws)) ->
                if w == wpercent then go (NoChar, ws)
                    else if w == wplus
                            then Just (wspace, (NoPercent, ws))
                            else Just (w, (NoPercent, ws))
            (NoChar, Just (w, ws)) ->
                case hexVal w of
                    Nothing -> Just (wpercent, (NoPercent, x))
                    Just v -> go (OneChar (w, v), ws)
            (OneChar (w1, v1), Just (w2, ws)) ->
                case hexVal w2 of
                    Nothing ->
                        Just (wpercent, (NoPercent, w1 `cons` x))
                    Just v2 -> Just (combine v1 v2, (NoPercent, ws))
    c2w :: Char -> Word8
    c2w = toEnum . fromEnum
    wequal = c2w '='
    wand = c2w '&'
    wpercent = c2w '%'
    w0 = c2w '0'
    w9 = c2w '9'
    wa = c2w 'a'
    wf = c2w 'f'
    wA = c2w 'A'
    wF = c2w 'F'
    wspace = c2w ' '
    wplus = c2w '+'
    hexVal w
        | w0 <= w && w <= w9 = Just $ w - w0
        | wa <= w && w <= wf = Just $ w - wa + 10
        | wA <= w && w <= wF = Just $ w - wA + 10
        | otherwise = Nothing
    combine :: Word8 -> Word8 -> Word8
    combine a b = shiftL a 4 .|. b

data DecodeHelper = NoPercent | NoChar | OneChar (Word8, Word8)
