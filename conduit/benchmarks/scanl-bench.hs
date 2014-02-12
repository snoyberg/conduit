import Criterion.Main
import Data.Conduit
import qualified Data.Conduit.List as CL
import qualified Data.Foldable as F
import Control.Monad (replicateM_)
import Data.Functor.Identity

test name f =
    bench name $ flip nf 200 $ \i -> runIdentity (replicateM_ i (yield ()) $= f (\_ _ -> ((), Nothing)) () $= CL.catMaybes $$ CL.sinkNull)

cScanl :: Monad m => (a -> s -> (s, b)) -> s -> Conduit a m b
cScanl step = loop where
    loop state =
        do ma <- await
           case ma of
             Nothing -> return ()
             Just a -> let ~(newState, b) = step a state in yield b >> loop newState

scanlOrig :: Monad m => (a -> s -> (s,b)) -> s -> Conduit a m b
scanlOrig f =
    loop
  where
    loop s = await >>= F.mapM_ go
      where
        go a = case f a s of
                 (s',b) -> yield b >> loop s'

scanlOrigTweaked :: Monad m => (a -> s -> (s,b)) -> s -> Conduit a m b
scanlOrigTweaked f =
    loop
  where
    loop s = await >>= maybe (return ()) go
      where
        go a = case f a s of
                 (s',b) -> yield b >> loop s'

main = defaultMain
    [ test "Data.Conduit.List.scanl" CL.scanl
    , test "Miguel's" cScanl
    , test "orig" scanlOrig
    , test "orig tweaked" scanlOrigTweaked
    ]
