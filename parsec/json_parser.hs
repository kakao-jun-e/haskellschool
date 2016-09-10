import Text.ParserCombinators.Parsec
import Control.Monad

matchTrue :: Parser String
matchTrue = string "true"

alwaysTrue :: Parser Bool
alwaysTrue = pure True

boolTrue :: Parser Bool
boolTrue = matchTrue *> alwaysTrue

matchFalse :: Parser String
matchFalse = string "false"

alwaysFalse :: Parser Bool
alwaysFalse = pure False

boolFalse :: Parser Bool
boolFalse = matchFalse *> alwaysFalse

bool :: Parser Bool
bool = boolTrue <|> boolFalse

stringLiteral :: Parser String
stringLiteral = do
  char '"'
  str <- many (noneOf "\"")
  char '"'
  return str

data JSONVal = Bool Bool
             | String String
             | Number Float
             | Array [JSONVal]
             | Object [(String, JSONVal)]
             deriving Show

parseBool :: Parser JSONVal
parseBool = lexeme (Bool <$> bool)

parseString :: Parser JSONVal
parseString = lexeme (String <$> stringLiteral)

parseNumber :: Parser JSONVal
parseNumber = liftM (Number . read) $ many1 (digit <|> char '.')

comma = lexeme $ char ','
colon = lexeme $ char ':'

array :: Parser [JSONVal]
array = do
  (lexeme $ char '[')
  entry <- sepBy parseJson comma
  (lexeme $ char ']')
  return entry

parseArray :: Parser JSONVal
parseArray = Array <$> array

objectEntry :: Parser (String, JSONVal)
objectEntry = do
      key <- stringLiteral
      colon
      value <- parseJson
      return (key, value)

parseObject :: Parser JSONVal
parseObject = do
   (lexeme $ char '{')
   obj <- sepBy (lexeme objectEntry) comma
   (lexeme $ char '}')
   return $ Object obj

parseJson :: Parser JSONVal
parseJson = parseBool
        <|> parseString
        <|> parseNumber
        <|> parseArray
        <|> parseObject

ws :: Parser String
ws = many (oneOf " \t\n")

lexeme p = p <* ws
