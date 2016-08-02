module ChapterExercises where
import Data.Functor.Identity
import Control.Monad.Trans.State (state, State, runStateT, runState)
import Control.Monad.Trans.State.Lazy hiding (get, put, modify)

get :: State s s
get = state $  (,) <$> id <*> id

put :: State s ()
put = state $ \s -> ((),s)

exec :: State s a -> s -> s
exec (StateT sa)   = snd . runIdentity . sa

eval :: State s a -> s -> a
eval (StateT sa)  = fst . runIdentity . sa

modify :: (s -> s ) -> State s ()
modify f = state $ \s ->((),f s)
