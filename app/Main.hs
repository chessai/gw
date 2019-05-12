{-# language DerivingStrategies #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language LambdaCase #-}
{-# language OverloadedStrings #-}
{-# language ScopedTypeVariables #-}
{-# language ViewPatterns #-}

module Main
  ( main
  ) where

import System.Environment (getArgs)
import qualified Data.List as List
import qualified System.Posix.Process as P

main :: IO ()
main = parseArgs >>= runCmd

parseArgs :: IO Cmd
parseArgs = getArgs >>= \case
  [] -> pure Help
  ("-v":_) -> pure Version
  ("--version":_) -> pure Version
  ("-h":_) -> pure Help
  ("--help":_) -> pure Help
  ("-p":xs) -> go Pure xs
  ("--pure":xs) -> go Pure xs
  xs -> go Impure xs
  where
    go :: Purity -> [String] -> IO Cmd
    go p = \case
      [] -> pure BadCmd
      (x:xs) -> case readGhc x of
        Left str -> do
          putStr (str ++ "\n")
          pure BadCmd
        Right g -> pure $ GhcWith p g (PkgSet xs)

ghcWith :: Ghc -> PkgSet -> String
ghcWith (showGhc -> ghc) (showPkgSet -> pkgs) = mconcat
  [ "haskell.packages."
  , ghc
  , ".ghcWithPackages (pkgs: with pkgs; [ "
  , pkgs
  , " ])"
  ]

ghcWithIO :: Ghc -> PkgSet -> Purity -> IO ()
ghcWithIO g pkgs p
  = P.executeFile "nix-shell" True (p' ++ ["-p",ghcWith g pkgs]) Nothing 
  where
    p' = case p of
      Pure -> ["--pure"]
      Impure -> []

data Cmd
  = Help
  | Version
  | GhcWith Purity Ghc PkgSet
  | BadCmd

runCmd :: Cmd -> IO ()
runCmd = \case
  Help -> putStr help
  Version -> putStr version
  GhcWith p g pkgs -> ghcWithIO g pkgs p
  BadCmd -> putStr badCmd

help,version,badCmd :: String
help = mconcat
  [ "\n"
  , "    gw - a utility for entering a nix-shell using ghcWithPackages\n\n"
  , "    Usage: gw <OPTIONS> <GHC> <PKGS>\n\n"
  , "    Available options:\n"
  , "      -h,--help            Display this help menu\n"
  , "      -p,--pure            Enter a pure nix-shell\n"
  , "      -v,--version         Display the version of gw\n"
  , "\n"
  ]
version = "gw 0.1"
badCmd = mconcat
  [ "\n"
  , "Malformed command. Help menu:\n"
  , help
  ]

readGhc :: String -> Either String Ghc
readGhc = \case
  "ghc710" -> Right Ghc710
  "ghc801" -> Right Ghc801
  "ghc802" -> Right Ghc802
  "ghc821" -> Right Ghc821
  "ghc822" -> Right Ghc822
  "ghc841" -> Right Ghc841
  "ghc842" -> Right Ghc842
  "ghc843" -> Right Ghc843
  "ghc844" -> Right Ghc844
  "ghc861" -> Right Ghc861
  "ghc862" -> Right Ghc862
  "ghc863" -> Right Ghc863
  "ghc864" -> Right Ghc864
  "ghc865" -> Right Ghc865
  "ghcHEAD" -> Right GhcHead
  x -> Left $ "GHC arg \"" ++ x ++ "\" passed is not one of " ++ ghcSet

showGhc :: Ghc -> String
showGhc = \case
  Ghc710 -> "ghc710"
  Ghc801 -> "ghc801"
  Ghc802 -> "ghc802"
  Ghc821 -> "ghc821"
  Ghc822 -> "ghc822"
  Ghc841 -> "ghc841"
  Ghc842 -> "ghc842"
  Ghc843 -> "ghc843"
  Ghc844 -> "ghc844"
  Ghc861 -> "ghc861"
  Ghc862 -> "ghc862"
  Ghc863 -> "ghc863"
  Ghc864 -> "ghc864"
  Ghc865 -> "ghc865"
  GhcHead -> "ghcHEAD"
 
data Ghc
  = Ghc710
  | Ghc801
  | Ghc802
  | Ghc821
  | Ghc822
  | Ghc841
  | Ghc842
  | Ghc843
  | Ghc844
  | Ghc861
  | Ghc862
  | Ghc863
  | Ghc864
  | Ghc865
  | GhcHead
  deriving stock (Eq,Ord,Enum,Bounded,Show)

ghcSet :: String
ghcSet = mconcat
  [ "{"
  , (List.intercalate ", " (map showGhc [minBound..maxBound :: Ghc]))
  , "}"
  ]

newtype PkgSet = PkgSet [String]
  deriving newtype (Eq,Show)

showPkgSet :: PkgSet -> String
showPkgSet (PkgSet set) = List.unwords set

data Purity = Pure | Impure
  deriving stock (Eq,Show)
