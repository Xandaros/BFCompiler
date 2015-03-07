module BF.Syntax
    ( BFInstruction(..)
    ) where

data BFInstruction = IncPtr
                   | DecPtr
                   | IncVal
                   | DecVal
                   | Output
                   | Input
                   | Loop [BFInstruction]
                   deriving (Show)

