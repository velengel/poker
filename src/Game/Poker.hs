--https://gist.github.com/tokiwoousaka/b471aa0efed725c6a05d/revisions
module Game.Poker
  ( module Game.Poker.Hands
  , module Game.Poker.Cards
  , simpleGame
  ) where
import Cards
import Hands
import System.Random.Shuffle
import Data.List
import Control.Monad
import Control.Applicative
import Data.Char
import Safe
import Data.Maybe

type DiscardList = [Card]
type Deck = [Card]

{-main :: IO()
{-main = do
  hand <- randomHand
  res <- return $ judgePoker hand
  print $ show hand ++ " -> " ++ show res-}

main = do
  forM_ [1..500] $ \i -> do
    hand <- randomHand
    res <- return $ judgePoker hand
    putStrLn $ show i ++ "   " ++ show hand ++ " -> " ++ show res
-}

simpleGame :: IO ()
simpleGame = do
  putStrLn "----------------"
  putStrLn "--simple poker--"
  putStrLn "----------------"
  deck <- shuffleM allCards
  case getHand deck of
    Nothing -> error "unexpected error : gethand in simplegame"
    Just res -> matchPoker res
  ynQuestion "-- do you play poker again?" main (putStrLn "-- goodbye")

data Player = Player | Enemy deriving Eq

showPlayerName :: Player -> String
showPlayerName Player = "you"
showPlayerName Enemy = "enemy"

matchPoker :: (Hand, Deck) -> IO ()
matchPoker (mhand, deck) = do
  (mres, ndeck, nmhand) <- playPoker mhand deck Player
  case getHand ndeck of
    Nothing -> error "unexpected error : getHand in matchPoker"
    Just (ehand, odeck) -> do
      (eres, _, nehand) <- playPoker ehand odeck Enemy
      printResult nmhand nehand mres eres



playPoker :: Hand -> Deck -> Player -> IO ((PokerHand, Card), Deck, Hand)
playPoker hand deck player = do
  discards <- if player == Player
    then inputDisuse hand
    else aiDisuse hand
  case drawHand deck discards hand of
    Nothing -> error "unexpected error"
    Just (nhand, ndeck) -> do
      let res = pokerHand nhand
      return (res, ndeck, nhand)

inputDisuse :: Hand -> IO DiscardList
inputDisuse hand = do
  printHand [] hand Player
  putStrLn "--select discards"
  gotDisuse <- getDiscardList hand
  case gotDisuse of
    Nothing -> do
      putStrLn "-- input digits from 1 to 5"
      inputDisuse hand
    Just disuses -> do
    printHand disuses hand Player
    ynQuestion "-- you : check it." (return disuses) (inputDisuse hand)

----

aiDisuse :: Hand -> IO DiscardList
aiDisuse hand = do
  let res = alSelectDiscards　hand
  printHand res hand Enemy
  putStrLn "-- enemy: That's OK."
  return res

printResult :: Hand -> Hand -> (PokerHand, Card) -> (PokerHand, Card ) -> IO ()
printResult mhand ehand mres@(mph, mcard) eres@(eph, ecard) = do
  putStrLn " ***** Result!! *****"
  printHand [] mhand Player
  printHand [] ehand Enemy
  putStrLn $ concat ["Your cards are ", show mph, " and, the most strongest card is ", show mcard, "."]
  putStrLn $ concat ["Enemy's cards are ", show eph, " and, the most strongest card is ", show ecard, "."]
  case judgeVictory mres eres of
    LT -> putStrLn "You lose..."
    EQ -> putStrLn "Draw."
    GT -> putStrLn "You win!!"

printHand :: DiscardList -> Hand -> Player -> IO()
printHand dis hand player = 
  putStrLn $ "-- " ++ showPlayerName player ++ " cards : " ++ showChangeHand dis hand



ynQuestion :: String -> IO a -> IO a -> IO a
ynQuestion s yes no = do
  putStrLn $ s ++ "(y/n)"
  input <- getLine
  case input of
    "y" -> yes
    "n" -> no
    _ -> do
      putStrLn "-- input 'y' or 'n'"
      ynQuestion s yes no

showChangeHand :: DiscardList -> Hand -> String
showChangeHand dis h = let
  judge x = if elem x dis then " " ++ show x ++ " " else "[" ++ show x ++ "]"
  in concat $ map judge (fromHand h)


randomHand :: IO (Maybe Hand)
randomHand = do
  shuffled <- shuffleM allCards
  return . toHand . take 5 $ shuffled

judgePoker :: Maybe Hand -> Maybe (PokerHand, Card)
judgePoker h = do
  i <- h
  return $ pokerHand i



getHand :: Deck -> Maybe (Hand, Deck)
getHand deck = do
  hand <- toHand . take 5 $ deck
  return (hand, drop 5 deck)

drawHand :: Deck -> DiscardList -> Hand -> Maybe (Hand, Deck)
drawHand deck dis h = let
  nl = filter (flip notElem dis) (fromHand h)
  nr = drop (5 - length nl) deck
  {-in do
    hand <- toHand . take 5 $ nl ++ deck
    ndeck <- return nr
    return (hand, ndeck)-}
  in (,) <$> toHand (take 5 $ nl ++ deck) <*> Just nr

toIntList :: String -> Maybe [Int]
toIntList str = if and $ map isDigit str then Just $ reads str else Nothing
  where
    reads :: String -> [Int]
    reads = map $ read . (:[])

selectByIndexes :: [a] -> [Int] -> Maybe [a]
selectByIndexes l = sequence . map ((atMay l).(subtract 1))

getDiscardList :: Hand -> IO (Maybe DiscardList)
getDiscardList h = do
  input <- getLine
  return $ do
    intList <- toIntList input
    res <- selectByIndexes (fromHand h) intList
    return res
 ---- 

allNOfKinds :: Hand -> [Card]
allNOfKinds hand = concat . concat 
  $ catMaybes [nOfKindHint 2 hand, nOfKindHint 3 hand, nOfKindHint 4 hand]

nOfKindDiscards :: Hand -> DiscardList
nOfKindDiscards hand = filter (flip notElem $ allNOfKinds hand) $ fromHand hand

alSelectDiscards :: Hand -> DiscardList
alSelectDiscards hand = 
  case straightHint hand `mplus` flushHint hand *> Just [] of
    Nothing -> nOfKindDiscards hand
    Just xs -> xs

judgeVictory :: (PokerHand, Card) -> (PokerHand, Card) -> Ordering 
judgeVictory l r = compare (pullStrength l) (pullStrength r)
  where 
    pullStrength :: (PokerHand, Card) -> (PokerHand, Int)
    pullStrength = fmap cardStrength