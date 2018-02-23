module Main where

import Data.Char
import Data.List
import Data.Function
import Data.Monoid
import Control.Monad
import Control.Applicative
import System.IO

type Title = String
type Director = String
type Year = Int
type User = String

data Film = Film {
  title::Title,
  director::Director,
  year::Year,
  likes::[User],
  dislikes::[User]
  }

instance Show Film where
  show (Film t d y l ds) = let showLikes l = concat . intersperse ", " $ map show l in
    intersperse "\n" [show t, show d,show y, showLikes l, showLikes ds] >>= id

-------------------------------------------------------------------------------
-- Nano Parser for database
-------------------------------------------------------------------------------

newtype Parser a = Parser { parse :: String -> [(a,String)] }

runParser :: Parser a -> String -> a
runParser m s =
  case parse m s of
    [(res, [])] -> res
    [(_, rs)]   -> error $ "Parser did not consume entire stream. & remainder is" ++ rs
    _ -> error $ "Parser error."

instance Functor Parser where
  fmap f (Parser cs) = Parser (\s -> [(f a, b) | (a, b) <- cs s])

instance Applicative Parser where
  pure a = Parser (\s -> [(a,s)])
  (Parser cs1) <*> (Parser cs2) = Parser (\s -> [(f a, s2) | (f, s1) <- cs1 s, (a, s2) <- cs2 s1])

instance Monad Parser where
  (>>=)  p f = Parser $ \s -> concatMap (\(a, s') -> parse (f a) s') $ parse p s

instance MonadPlus Parser where
  mzero = Parser (\cs -> [])
  mplus p q = Parser (\s -> parse p s ++ parse q s)

instance Alternative Parser where
  empty = mzero
  (<|>) p q = Parser $ \s ->
    case parse p s of
      []     -> parse q s
      res    -> res
  some v = some_v
    where
      many_v = some_v <|> pure []
      some_v = (:) <$> v <*> many_v
  many v = many_v
    where
      many_v = some_v <|> pure []
      some_v = (:) <$> v <*> many_v

-------------------------------------------------------------------------------
-- Combinators
-------------------------------------------------------------------------------

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = item >>= \c ->
  if p c
  then return c
  else mzero
  where
    item :: Parser Char
    item = Parser $ \s ->
      case s of
      []     -> []
      (c:cs) -> [(c,cs)]

oneOf :: [Char] -> Parser Char
oneOf s = satisfy (flip elem s)

notOneOf :: [Char] -> Parser Char
notOneOf s = satisfy (not . flip elem s)

char :: Char -> Parser Char
char c = satisfy (c ==)

name :: Parser String
name = some (satisfy $ not . isPunctuation)

string :: String -> Parser String
string [] = return []
string (c:cs) = do { char c; string cs; return (c:cs)}

token :: Parser a -> Parser a
token p = do { a <- p; spaces ; return a}

reserved :: String -> Parser String
reserved s = token (string s)

digit :: Parser Char
digit = satisfy isDigit

number :: Parser Int
number = do
  s <- string "-" <|> return []
  cs <- some digit
  return $ read (s ++ cs)

spaces :: Parser String
spaces = many $ oneOf " \n\r"

qoutesPara :: Parser String -> Parser String
qoutesPara m = do
  char '\"'
  n <- m
  char '\"'
  return $ n

endBy :: Parser a -> Parser end -> Parser a
endBy p end = do
  x <- p
  end
  return x
sepBy :: Parser a -> Parser sep -> Parser [a]
sepBy p sep = do
  x <- p
  xs <- many $ sep >> p
  return (x:xs)

emptyList:: Char -> Parser [a]
emptyList  c = do
  n <- char c
  return []

-------------------------------------------------------------------------------
-- Main Project
-------------------------------------------------------------------------------


main :: IO ()
main = do
    films <- loadDb
    demo films 2

runFilm :: [Film] -> IO()
runFilm fs = do
  n <- (read :: String -> Int) <$> getLine
  demo fs n

demo :: [Film] -> Int -> IO ()
demo f 1 = do
  let fm = newFilm "Sherlock Gnomes" "John Stevenson" 2018
  saveNewFilm fm
  print f
  runFilm (fm:f)
demo f 2 = print f >> runFilm f
demo f 3 = do
  print $ findByDirector "Ridley Scott" f
  runFilm f
demo f 4 = do
  print $ filter (\x-> (getRate x) > 0.75) f
  runFilm f
demo f 5 = do
  print $ foldl (\acc v -> acc + (getRate v)) 0 filmByRidley
  runFilm f
  where
    filmByRidley = filter (\x-> (director x) == "Ridley Scott") f
demo f 6 = do
  print $ map title $ filter (\x -> "Emma" `elem` likes x || "Emma" `elem` dislikes x) f
  runFilm f
demo f 7 = do
  let fm = likeFilm "Emma" "Avatar" f
  print fm
  save fm
  runFilm fm
demo f 71 = do
  let fm = likeFilm "Emma" "Titanic" f
  print fm
  save fm
  runFilm fm
demo f 72 = do
  let fm = disLikeFilm "Emma" "Jaws" f
  print fm
  save fm
  runFilm fm
demo f 8 = do
  print $ sortBy (on compare (\x -> getRate x )) (getRange 2000 2006 f)
  runFilm f
demo f _ = do
  save f
  putStrLn "None support demo number save and exist"

-- demo 1  = putStrLn all films after adding 2018 film "Sherlock Gnomes"
-- --         directed by by "John Stevenson" to testDatabase
-- demo 2  = putStrLn (filmsAsString testDatabase)
-- demo 3  = putStrLn all films by "Ridley Scott"
-- demo 4  = putStrLn all films with website rating >= 75%
-- demo 5  = putStrLn average website rating for "Ridley Scott"
-- demo 6  = putStrLn titles of films rated by "Emma" (with likes/dislikes)
-- demo 7  = putStrLn all films after "Emma" says she likes "Avatar"
  -- demo 71 = putStrLn all films after "Emma" says she likes "Titanic"
-- demo 72 = putStrLn all films after "Emma" says she dislikes "Jaws"
-- demo 8  = films between 2000 and 2006 inclusive sorted by website rating

loadDb :: IO [Film]
loadDb = do
  fileHandle <- openFile "testDatabase.txt" ReadWriteMode
  runParser (many filmParser) <$> hGetContents fileHandle

filmParser :: Parser Film
filmParser = do
  t <- endBy (qoutesPara (many $ notOneOf "\"")) (char '\n')
  d <- endBy (qoutesPara (many $ notOneOf "\"")) (char '\n')
  y <- endBy number (char '\n')
  l <- endBy (sepBy (qoutesPara name) (reserved ", ") <|> pure []) (char '\n')
  dl <- endBy (sepBy (qoutesPara name) (reserved ", ") <|> pure []) (char '\n')
  char '\n' <|> pure '\n'
  return $ Film t d y l dl

save :: [Film] -> IO()
save f = writeFile "testDatabase.txt" $ (concat $ intersperse "\n\n" (show <$> f)) ++ "\n"

newFilm :: Title -> Director -> Year -> Film
newFilm t d y = Film t d y [] []

saveNewFilm :: Film -> IO()
saveNewFilm f = appendFile "testDatabase.txt" $ '\n': (show f) ++ "\n"

findByDirector :: Director ->[Film] -> [Film]
findByDirector d = filter (\x -> director x == d)

getRate :: Film -> Float
getRate f = let likeListSize = length $ likes f
                dislikeListSize = length $ dislikes f
            in
              (fromIntegral likeListSize) / fromIntegral (likeListSize + dislikeListSize)

getRange :: Year -> Year -> [Film] -> [Film]
getRange s e f = filter (\x -> year x >= s && year x <= e) f

likeFilm :: User -> Title -> [Film] -> [Film]
likeFilm u t fs = p ++ (newFilm : (tail n))
  where
    (p,n) = span (\x -> title x /= t) fs
    f = head n
    existLikes = likes f
    newLikes = insertIfNotExist u existLikes
    newDislikes = findAndDelete u (dislikes f) []
    newFilm = f {likes = newLikes, dislikes = newDislikes}

disLikeFilm :: User -> Title -> [Film] -> [Film]
disLikeFilm u t fs = p ++ (newFilm : (tail n))
  where
    (p,n) = span (\x -> title x /= t) fs
    f = head n
    existDisLikes = dislikes f
    newDislikes = insertIfNotExist u existDisLikes
    newLikes = findAndDelete u (likes f) []
    newFilm = f {likes = newLikes, dislikes = newDislikes}

insertIfNotExist :: User -> [User] -> [User]
insertIfNotExist u l = if u `elem` l then l else u: l

findAndDelete :: User -> [User] -> [User] -> [User]
findAndDelete _ [] acc = acc
findAndDelete u (x:xs) acc = if x == u then
  findAndDelete u xs acc
  else
  findAndDelete u xs (x:acc)
