definition module monad

/*
	Pieter Koopman, pieter@cs.ru.nl
	Advanced Programming, week 4, 2016
*/

import StdMisc

class Functor f where
	fmap :: (a->b) (f a) -> (f b)
	(<$>) infixl 4 :: (a->b) (f a) -> (f b) | Functor f
 	(<$>) f x :== fmap f x

class Applicative f | Functor f where
	pure :: a -> f a
	(<*>) infixl 4 :: (f (a->b)) (f a) -> f b

class Monad m | Applicative m where
	bind :: (m a) (a->m b) -> m b
	(>>=) infixl 1 :: (m a) (a->m b) -> m b | Monad m
	(>>=) a f :== bind a f
	(>>|) infixl 1 :: (m a)    (m b) -> m b | Monad m
	(>>|) a b :== a >>= \_.b
	rtrn :: a -> m a | Monad m
	rtrn a :== pure a

class fail m | Applicative m where
	fail :: m a
	guard :: Bool -> m a | fail m
	guard b :== if b (pure undef) fail

class OrMonad m where
	(<|>) infixl 0 :: (m a) (m a) -> m a

