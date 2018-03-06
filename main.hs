import System.Environment

main = getArgs >>= \args ->
    case args of
      [file] -> readFile file >>= \x -> putStr (interpreter x)
      _ -> putStr "Wrong number of inputs"

interpreter = \input -> reduce . tokenize $ input

data Token = Operator String | Number String

data Tree a b = OperatorNode a Tree a b Tree a b | NumberNode b

tokenize :: String -> [Token]
tokenize = \input -> [Number "1"]

consume :: [Token] -> (Tree, [Token])
consume = \tokens ->
    case tokens of
      op@(Operator _) : first@(Number _) : second@(Number _) : xs -> do
          (Node op first second, xs)
      op@(Operator _)  : _ -> do
          let (tree, moreTokens) = consume tokens
              (secondTree, evenMoreTokens) = consume moreTokens
           in (Node op tree secondTree, evenMoreTokens)


reduce :: [Token] -> String
reduce = \tokens ->
    case tokens of
      Operator t:_ -> t
      Number t:_ -> t
      _ -> "asdf"
