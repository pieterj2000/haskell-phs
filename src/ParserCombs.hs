{-# LANGUAGE InstanceSigs #-}
module ParserCombs (
    Parser(..)
) where



data Error

newtype Parser i a = Parser { parse :: [i] -> Either Error (a, [i]) }

instance Functor (Parser i) where
  fmap :: (a -> b) -> Parser i a -> Parser i b
  fmap f (Parser p) = Parser $ \input -> 
    case p input of 
        Left e -> Left e
        Right (x, rest) -> Right (f x, rest)


