import Control.Monad.State.Lazy

tick :: State Int Int
tick = do
	n <- get
	put (n+1)
	return n
