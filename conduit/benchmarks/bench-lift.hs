import           Control.Monad.Trans.State.Strict
import           Criterion.Main
import           Data.Conduit
import           Data.Conduit.Internal            (ConduitM (..), Pipe (..))
import qualified Data.Conduit.Lift                as CLift
import qualified Data.Conduit.List                as CL
import           Data.Functor.Identity

main :: IO ()
main = defaultMain
    [ bgroup "Strict StateT" $
        let sink :: Sink Int (State Int) ()
            sink = CL.mapM_ $ modify . (+)
            src = mapM_ yield [1..1000 :: Int]
         in [ bench "generic" $ whnf (\i -> runIdentity $ src $$ CLift.execStateC i sink) 0
            , bench "specialized" $ whnf (\i -> runIdentity $ src $$ execStateC i sink) 0
            ]
    ]

runStateC
  :: Monad m =>
     s -> ConduitM b o (StateT s m) r -> ConduitM b o m (r, s)
runStateC s0 (ConduitM c0) =
    ConduitM (go s0 c0)
  where
    go s (Done r) = Done (r, s)
    go s (PipeM (StateT f)) = PipeM $ do
        (c, s') <- f s
        return $ go s' c
    go s (Leftover c i) = Leftover (go s c) i
    go s (HaveOutput c f o) = HaveOutput (go s c) (evalStateT f s) o
    go s (NeedInput x y) = NeedInput (go s . x) (go s . y)

execStateC
  :: Monad m =>
     b -> ConduitM b1 o (StateT b m) () -> ConduitM b1 o m b
execStateC s p = fmap snd $ runStateC s p
