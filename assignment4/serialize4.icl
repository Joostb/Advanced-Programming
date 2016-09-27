module serialize4

import StdEnv, StdMaybe, monad

/*
	Pieter Koopman, pieter@cs.ru.nl
	Advanced Programming, week 4, 2016
*/

// ---

:: State s a = S (s -> (Maybe a,s))

unS :: (State s a) -> s -> (Maybe a,s)
unS (S f) = f

instance Functor (State s) where
	fmap f (S g) = S (\s.case g s of
                      (Nothing, xs) = (Nothing, xs) 
                      (Just a, xs)  = (Just (f a), xs))
instance Applicative (State s) where
  pure a = S \s.(Just a,s)
  (<*>) (S f) (S x) = S (\s. case f s of
                              (Just f1, xs) = case x xs of
                                              (Just x1, xss) = (Just (f1 x1), xss)
                                              (Nothing, xss) = (Nothing,xss)
                              (Nothing,xs) = (Nothing, xs)
                        )
instance fail (State s) where
	fail = S \s.(Nothing,s)
instance Monad (State s) where
	bind (S a) f = S (\s. case a s of
                          (Just as, xs) = unS (f as) xs
                          (Nothing, xs) = (Nothing,xs))
instance OrMonad (State s) where
	(<|>) (S f) (S g) = S(\s. case f s of
                          (Nothing, _) = g s
                          _ = f s)

// ---

:: Serialized = {naam::[String]}

ser :: Serialized
ser = {naam=[]}

toStrings :: Serialized -> [String]
toStrings s = s.naam

:: Serialize a :== State Serialized a

// S (Serialized -> (Maybe a, Serialized))

wrt :: a -> Serialize String | toString a
wrt a = S (\ {naam} . (Just (toString a), {naam=[(toString a) : naam]} ))

// Serialized -> (Maybe String, Serialized)
rd :: Serialize String 
rd = S (\ {naam} . (case naam of 
                         [x:xss] = (Just x, {naam=xss})
                         [] = (Nothing, {naam=[]})
                   )
       )

// rd :: Serialized -> (Maybe String, Serialized)
//match :: a -> (Serialized -> Maybe String, Serialized)
match :: a -> Serialize a | toString a
match a = fail //rd >>= (\(Just x, xs). if ((toString a) == x) (return xs) fail ) <|> fail

//if read returns Just a, xs, we want to compare a and toString a
//    if that succeeds return the state else fail
//else fail

pred :: (String->Bool) -> Serialize String
pred f = fail //rd >>= (\(Just x, xs). if (f x) (pure xs) (Nothing, xs)) <|> fail


// ---

:: UNIT     = UNIT
:: EITHER a b = LEFT a | RIGHT b
:: PAIR   a b = PAIR a b
:: CONS   a   = CONS String a

:: Write a :== a -> Serialize String
:: Read a  :== Serialize a
 
class serialize a | isUNIT a where
  write :: a -> Serialize String
  read  :: Serialize a

class serialize1 t where
  write1 :: (Write a) (t a) -> Serialize String
  read1  :: (Read  a) -> Serialize (t a)

class serializeCONS a where
	writeCons :: (Write a) (CONS a) -> Serialize String
	readCons  :: String (Read a) -> Serialize (CONS a)

class serialize2 t where
  write2 :: (Write a) (Write b) (t a b) -> Serialize String
  read2  :: (Read  a) (Read  b) -> Serialize (t a b)

class isUNIT a :: a -> Bool
instance isUNIT UNIT where isUNIT _ = True
instance isUNIT a    where isUNIT _ = False

instance serialize Bool where
  write b = fail
  read = fail

instance serialize Int where
	write i = fail
	read = fail

instance serialize String where
	write s = wrt s
	read = fail

instance serialize UNIT where
	write _ = fail
	read = fail

instance serializeCONS UNIT where
	writeCons wa (CONS name a) = fail
	readCons name ra = fail
 
instance serializeCONS a where
	writeCons wa (CONS name a) = fail
	readCons name ra =fail
 
instance serialize2 EITHER where
  write2 wa wb (LEFT  a) = fail
  write2 wa wb (RIGHT b) = fail
  read2 ra rb = fail

instance serialize2 PAIR where
  write2 wa wb (PAIR a b) = fail
  read2 ra rb = fail

// ---

:: ListG a :== EITHER (CONS UNIT) (CONS (PAIR a [a]))

fromList :: [a] -> ListG a
fromList []  = LEFT  (CONS NilString  UNIT)
fromList [a:x] = RIGHT (CONS ConsString (PAIR a x))

toList :: (ListG a) -> [a]
toList (LEFT  (CONS NilString  UNIT)) = []
toList (RIGHT (CONS ConsString (PAIR a x))) = [a:x]

NilString :== "Nil"
ConsString :== "Cons"

instance serialize [a] | serialize a where
 write a = write1 write a
 read    = read1  read

instance serialize1 [] where
	write1 writea l = fail
	read1  reada = fail
// ---

:: Bin a = Leaf | Bin (Bin a) a (Bin a)

:: BinG a :== EITHER (CONS UNIT) (CONS (PAIR (Bin a) (PAIR a (Bin a))))

fromBin :: (Bin a) -> BinG a
fromBin Leaf = LEFT (CONS LeafString UNIT)
fromBin (Bin l a r) = RIGHT (CONS BinString (PAIR l (PAIR a r)))

toBin :: (BinG a) -> Bin a
toBin (LEFT (CONS _ UNIT)) = Leaf
toBin (RIGHT (CONS _ (PAIR l (PAIR a r)))) = Bin l a r

LeafString :== "Leaf"
BinString :== "Bin"

instance == (Bin a) | == a where
  (==) Leaf Leaf = True
  (==) (Bin l a r) (Bin k b s) = l == k && a == b && r == s
  (==) _ _ = False

instance serialize (Bin a) | serialize a where
	write b = fail
	read = fail

instance serialize1 Bin where
	write1 writea b = fail
	read1  reada    = fail
// ---

:: Coin = Head | Tail
:: CoinG :== EITHER (CONS UNIT) (CONS UNIT)

fromCoin :: Coin -> CoinG
fromCoin Head = LEFT (CONS "Head" UNIT)
fromCoin Tail = RIGHT (CONS "Tail" UNIT)

toCoin :: CoinG -> Coin
toCoin (LEFT (CONS _ UNIT)) = Head
toCoin (RIGHT (CONS _ UNIT)) = Tail

instance == Coin where
  (==) Head Head = True
  (==) Tail Tail = True
  (==) _    _    = False

instance serialize Coin where
	write c = fail
	read    = fail

// ---

Start = 
  [test True
  ,test False
  ,test 0
  ,test 123
  ,test -36
  ,test Head
  ,test Tail
  ,test [42]
  ,test [0..4]
  ,test [[True],[]]
  ,test (Bin Leaf True Leaf)
  ,test [Bin (Bin Leaf [1] Leaf) [2] (Bin Leaf [3] (Bin Leaf [4,5] Leaf))]
  ,test [Bin (Bin Leaf [1] Leaf) [2] (Bin Leaf [3] (Bin (Bin Leaf [4,5] Leaf) [6,7] (Bin Leaf [8,9] Leaf)))]
  ,test Head
  ,test Tail
  ,["End of the tests.\n"]
  ]

test :: a -> [String] | serialize, == a
test a = toStrings (snd ((unS t) ser)) where
 t
 	=   write a
	>>| read
	>>= \b. guard (a == b)
	>>| write "Oke "
	<|> write "Failure "

