import BF.Parser
import Text.ParserCombinators.Parsec
import System.Environment

parsebf :: String -> String
parsebf a = case (parse parseFile "" a) of
                 Left a -> ""
                 Right b -> show b

main :: IO ()
main = interact parsebf
