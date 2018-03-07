import System.Environment
import Data.Char
import Debug.Trace

main = getArgs >>= \args ->
    case args of
      [file] -> readFile file >>= \x -> putStr (interpreter x)
      _ -> putStr "Wrong number of inputs"

interpreter = \input -> show . reduce . treeify . tokenize $ input

newtype Operator = Operator String
newtype Number = Number String
data Token = OperatorToken Operator | NumberToken Number
data Tree = OperatorNode Operator Tree Tree | NumberNode Number

tokenize :: String -> [Token]
tokenize input =
    case input of
      a:rest | elem a ['+', '*', '-', '/'] -> (OperatorToken (Operator [a])):tokenize rest
      a:rest | isDigit a || a == '.' ->
          tokenizeWithState rest (NumberToken (Number [a]))
      ' ':rest -> tokenize rest
      _:rest -> tokenize rest

tokenizeWithState :: String -> Token -> [Token]
tokenizeWithState input token =
    case input of
      a:rest | isDigit a || a == '.' ->
          case token of
            OperatorToken _ -> token:tokenizeWithState rest (NumberToken (Number [a]))
            NumberToken (Number b) -> token:tokenizeWithState rest (NumberToken (Number (b ++ [a])))
      rest -> token:tokenize rest

treeify :: [Token] -> Tree
treeify tokens =
    case consume tokens of
      (tree, _) -> tree

consume :: [Token] -> (Tree, [Token])
consume = \tokens ->
    case tokens of
      OperatorToken op : rest -> do
          let (tree, moreTokens) = consume rest
              (secondTree, evenMoreTokens) = consume moreTokens
           in (OperatorNode op tree secondTree, evenMoreTokens)
      NumberToken num : rest -> (NumberNode num, rest)

reduce :: Tree -> Double
reduce tree =
    case tree of
      NumberNode (Number n) -> read n
      OperatorNode op first second ->
          let f = reduce first
              s = reduce second
              Operator k = op
           in trace ("Computing :" ++ (show f) ++ k ++ (show s)) (case op of
                Operator "+" -> f + s
                Operator "*" -> f * s
                Operator "-" -> f - s
                Operator "/" -> f / s
                                                  )
