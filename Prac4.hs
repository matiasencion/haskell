data Nat = Zero | Succ Nat
    deriving (Show)

nat2int :: Nat -> Int
nat2int (Zero) = 0
nat2int (Succ n) = 1 + nat2int (n)

int2nat :: Int -> Nat
int2nat 0 = (Zero)
int2nat n = (Succ(int2nat(n-1)))

duplica :: Nat -> Nat
duplica (Zero) = (Zero)
duplica (Succ n) = Succ((Succ(duplica(n))))

suma :: Nat -> Nat -> Nat
suma (Zero) x = x
suma x (Zero) = x
suma (Succ n) (Succ m) = int2nat(1+1+nat2int(n)+nat2int(m))

predecesor :: Nat -> Nat
predecesor (Zero) = (Zero)
predecesor (Succ n) = n

foldN :: (a -> a) -> a -> Nat -> a
foldN h e Zero = e
foldN h e (Succ n) = h (foldN h e n)


