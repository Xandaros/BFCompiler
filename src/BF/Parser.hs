module BF.Parser (parseFile) where
import Text.ParserCombinators.Parsec
import Data.Maybe (catMaybes)
import Control.Monad

import BF.Syntax

parseInst :: Parser (Maybe BFInstruction)
parseInst = parseIncPtr <|> parseDecPtr <|> parseIncVal <|> parseDecVal <|> parseOutput <|> parseInput <|> parseLoop

parseInsts :: Parser [BFInstruction]
parseInsts = many (parseInst <|> parseComment) >>= return . catMaybes

parseFile :: Parser [BFInstruction]
parseFile = do
          a <- parseInsts
          eof
          return a

parseComment :: Parser (Maybe BFInstruction)
parseComment = noneOf "><+-.,[]" >> return Nothing

parseIncPtr, parseDecPtr, parseIncVal, parseDecVal,
        parseOutput, parseInput, parseLoop :: Parser (Maybe BFInstruction)
parseIncPtr = char '>' >> return (Just IncPtr)

parseDecPtr = char '<' >> return (Just DecPtr)

parseIncVal = char '+' >> return (Just IncVal)

parseDecVal = char '-' >> return (Just DecVal)

parseOutput = char '.' >> return (Just Output)

parseInput = char ',' >> return (Just Input)

parseLoop = do
          char '['
          a <- parseInsts
          char ']'
          return $ Just $ Loop a
