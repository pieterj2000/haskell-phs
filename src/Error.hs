{-# LANGUAGE InstanceSigs #-}
module Error (
    Error(..)
) where


data Error
    = ModuleFileName String String -- module name, path to file



instance Show Error where
  show :: Error -> String
  show (ModuleFileName mname path) = ""
