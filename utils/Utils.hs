{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE NoStarIsType #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

module Utils (
    mapLeft
, (.:)) where
import Data.Void




-- TODO verplaatsen naar iets van utils?
mapLeft :: (a -> b) -> Either a c -> Either b c
mapLeft f (Left x)  = Left $ f x
mapLeft _ (Right x) = Right x

infixr 8 .:
(.:) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(.:) = (.) . (.)


uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (a,b,c) = f a b c


fst3 :: (a, b, c) -> a
fst3 (a,b,c) = a
snd3 :: (a, b, c) -> b
snd3 (a,b,c) = b 
thd3 :: (a, b, c) -> c
thd3 (a,b,c) = c
first3 :: (a -> d) -> (a,b,c) -> (d,b,c)
first3 f (a,b,c) = (f a,b,c)
second3 :: (b -> d) -> (a,b,c) -> (a,d,c)
second3 f (a,b,c) = (a,f b,c)
third3 :: (c -> d) -> (a,b,c) -> (a,b,d)
third3 f (a,b,c) = (a,b,f c)




data Nat = Zero | Succ Nat
type Nat0 = Zero
type Nat1 = Succ Nat0
type Nat2 = Succ Nat1
type Nat3 = Succ Nat2
type Nat4 = Succ Nat3
type Nat5 = Succ Nat4

data Asdf (fase :: Nat) where
    Nul :: Asdf Nat0 
    Een :: Asdf Nat1
    Twee :: Asdf Nat2
    Drie :: Asdf Nat3
    Alles :: Asdf fase
    Mix :: Asdf fase1 -> Asdf fase2 -> Asdf (Min fase1 fase2)
    Lijst :: fase <= lijstfase => FaseList Asdf lijstfase -> Asdf fase
    Lijst' :: FaseList Asdf Nat2 -> Asdf Nat2


data FaseList a (fase :: Nat) where
    Nil :: forall fase a. FaseList a fase
    (:<) :: a fase -> FaseList a lfase -> FaseList a (Min fase lfase)
infixr 5 :<


deriving instance Show (FaseList Asdf fase)

-- instance Show (FaseList Asdf fase) where
--   show :: Show (a fase) => FaseList a fase -> String
--   show x = "[" ++ doe x
--     where
--         doe Nil = "]"
--         doe (a :< as) = show a ++ show as


deriving instance Show (Asdf fase)
--instance Show ((forall f. Asdf f -> r) -> r)

--f :: FaseList Asdf 
f :: Asdf Nat1
f = Lijst $ Twee :< Nil @Nat5


--g = Lijst' $  :< Nil @Nat2

b = show f



class (a :: Nat) <= (b :: Nat)
instance Zero <= a
instance a <= b => Succ a <= Succ b



type family Min (a :: Nat) (b :: Nat) :: Nat where
    Min Zero a = Zero
    Min a Zero = Zero
    Min (Succ a) (Succ b) = Succ (Min a b)

type family Heeft (a :: Nat) fase where
    Heeft Zero fase = fase
    Heeft (Succ a) fase = Succ (Heeft a fase)

nulofhoger :: Asdf (Heeft Nat0 fase) -> ()
nulofhoger Nul = ()
nulofhoger Een = ()

eenofhoger :: Asdf (Heeft Nat1 fase) -> ()
eenofhoger Twee =  ()
eenofhoger Nul =  ()

tweeofhoger ::  Asdf (Heeft Nat2 fase) -> ()
tweeofhoger Twee = ()
--tweeofhoger' a = eenofhoger' a
--tweeofhoger' a = const () $ idid @Nat2 @Nat0 @Nat4 a
--tweeofhoger' a = eenofhoger' $ idid @Nat2 @Nat0 @a a

tweeofhoger' :: Asdf (Heeft Nat2 fase) -> ()
tweeofhoger' = eenofhoger

eenofhoger' :: Asdf (Heeft Nat1 fase) -> ()
eenofhoger' = nulofhoger


doe1 = nulofhoger Nul
doe = eenofhoger Een
doe2 = tweeofhoger $ Mix Drie (Alles @Nat3)
doe2' = tweeofhoger' Twee


