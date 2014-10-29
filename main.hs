import BF.Parser
import Text.ParserCombinators.Parsec

main :: IO ()
main = case (parse parseFile "" "><+- rjgh,.[-]") of
	Left a -> print a
	Right b -> print b
