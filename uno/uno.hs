-- remember "stack ghci"
-- to install Gloss and its libraries, I used "stack install gloss" and it worked for Windows 10
-- TODO: let them pick their own card instead of manually picking one for them (this is where skill comes in), done with Gloss
-- TODO: also include the plain color cards in the action cards
import System.Random
import Data.List

type Deck = [Card]
type PlayerNum = Integer
type Name = [Char]
data Direction = Forward | Reverse 
 deriving Eq
data ActionCard = Pick2 | Pick4 | Switch | Skip 
 deriving (Eq, Show)
data Color = Blue | Red | Yellow | Green 
 deriving (Eq, Show)
data Card = RegularCard Integer Color | SpecialCard ActionCard Color 
 deriving (Eq, Show)
data Player = Participant PlayerNum Name Deck
 deriving (Eq, Show)
data GameException = NotEnoughPlayer | TooManyPlayer

-- entry point
initializeGame :: IO ()
initializeGame = do
 play_list <- createPlayerLists
 init_turn_no <- randomRIO (0, (length play_list) - 1)
 starting_card <- generateRandomCard
 takeTurn play_list init_turn_no starting_card Forward
 
compareCards :: Card -> Card -> Bool
compareCards (RegularCard card1 color1) (RegularCard card2 color2) = (card1 == card2) || (color1 == color2)
compareCards (RegularCard _ color1) (SpecialCard _ color2) = color1 == color2
compareCards (SpecialCard _ color1) (RegularCard _ color2) = color1 == color2
compareCards (SpecialCard action1 color1) (SpecialCard action2 color2) = (action1 == action2) || (color1 == color2)

checkValidity :: [Card] -> Card -> Bool
checkValidity [] card = False
checkValidity (x:xs) card = compareCards x card || checkValidity xs card

firstMatch :: [Card] -> Card -> Int -> (Int, Card)
firstMatch (x:xs) card num = 
 if ((compareCards x card) == True) then (num, x)
 else firstMatch xs card (num + 1)

-- set of getter methods for the player
extractPlayerNum :: Player -> Integer
extractPlayerNum (Participant playerno name deck) = playerno
extractPlayerName :: Player -> Name
extractPlayerName (Participant playerno name deck) = name
extractPlayerCardList :: Player -> [Card]
extractPlayerCardList (Participant playerno name deck) = deck

processSpecialCard :: [Player] -> Int -> Card -> Direction -> [Player] -- ([Player], Int, Direction)
processSpecialCard play_list turn_num card direction = []

-- mistake: we were only looking at the card that was on the deck, not the one played
takeTurn :: [Player] -> Int -> Card -> Direction -> IO ()
takeTurn [] _ _ _ = putStrLn "GAME OVER!"
takeTurn play_list turn_num card direction = do
 action_no <- requestAction play_list turn_num card
 case action_no of
  1 -> do
   let player = play_list !! turn_num
   let truth_value = checkValidity (extractPlayerCardList player) card
   if truth_value then do
    -- take a card from the player's current card list
    let player_card_list = extractPlayerCardList player
    let index_new_card = fst (firstMatch player_card_list card 0)
    let new_card = snd (firstMatch player_card_list card 0)
    let split_player_card_list = splitAt index_new_card player_card_list
    let new_player_card_list = fst split_player_card_list ++ tail (snd split_player_card_list)
    if (null new_player_card_list) then do
     -- force the game over state
     putStrLn $ "The winner of this round was: " ++ extractPlayerName player
     takeTurn [] 0 (RegularCard 0 Blue) direction
    else do
     -- modify the overall player list with all players in it
     let modified_player = Participant (extractPlayerNum player) (extractPlayerName player) new_player_card_list
     let player_tuple = splitAt turn_num play_list
     let new_play_list = fst player_tuple ++ [modified_player] ++ tail (snd player_tuple)
     -- CASE OF: what do I do with the already modified list, according to turn number and direction? (put the new_card into this function as well)
     case new_card of
      (RegularCard _ _) -> do
       if direction == Forward then do
        let new_turn_number = mod (turn_num + 1) (length new_play_list)
        takeTurn new_play_list new_turn_number new_card direction
       else do
        let new_turn_number = mod (turn_num - 1) (length new_play_list)
        takeTurn new_play_list new_turn_number new_card direction
   else do
    putStrLn $ id "No match was found, please select another action."
    takeTurn play_list turn_num card direction
  2 -> do
   -- append a card to the list
   let player = play_list !! turn_num
   let player_card_list = extractPlayerCardList player
   generatedCard <- generateRandomCard
   let new_player_card_list = player_card_list ++ [generatedCard]
   -- modify the overall player list with all players in it
   let modified_player = Participant (extractPlayerNum player) (extractPlayerName player) new_player_card_list
   let player_tuple = splitAt turn_num play_list
   let new_play_list = fst player_tuple ++ [modified_player] ++ tail (snd player_tuple)
   -- put an if statement here, Forward or Reverse
   let new_turn_number = mod (turn_num + 1) (length new_play_list)
   takeTurn new_play_list new_turn_number card direction
  _ -> do
   -- TODO: check for winning by default if there is one player left after someone quits
   print "Invalid number, please try again."

-- we want to get a number out of this, telling us our next action
requestAction :: [Player] -> Int -> Card -> IO Int
requestAction play_list turn_num card = do
 printPlayer (play_list !! turn_num)
 putStrLn $ "Here is the card on the deck: " ++ show card
 putStrLn "What would you like to do?"
 putStrLn "(1) Put a card down."
 putStrLn "(2) Pick a card from the deck."
 -- putStrLn "(3) Quit the game."
 str_num <- getLine
 let action_no = read str_num::Int
 return action_no

-- create the lists of all of the players and then give them 7 cards each, random cards will be drawn from a deck
createPlayerLists :: IO [Player]
createPlayerLists = do 
 putStrLn "How many players do you want to have?"
 numPlayers <- getLine
 let number = read numPlayers::Integer
 play_list <- addToList [] number 1
 return play_list
  
generateDeck :: [Card] -> Integer -> IO [Card]
generateDeck list 0 = return list
generateDeck list n = do 
 newCard <- generateRandomCard
 let newList = list ++ [newCard]
 generateDeck newList (n-1)
 
addToList :: [Player] -> Integer -> Integer -> IO [Player]
addToList list 0 _ = return list
addToList list a b = do
 putStrLn $ "What do you want the name of player " ++ show b ++ " to be?"
 nameOfPlayer <- getLine
 cardList <- generateDeck [] 7
 let newPlayerList = list ++ [Participant b nameOfPlayer cardList]
 addToList newPlayerList (a-1) (b+1)
  
printPlayer :: Player -> IO ()
printPlayer (Participant playno name deck) = do
 putStrLn $ id name ++ ", it's your turn! Here is your deck of cards:"
 print deck
  
generateRandomCard :: IO Card
generateRandomCard = do
 -- only make this go from 0 to 9 for the first phases of testing
 -- TODO: change this later on after confirming this first test (to game over)
 -- change the second parameter to 13 after you're done!
 cardnum <- randomRIO (0,9)::IO Integer
 colornum <- randomRIO (0,3)::IO Integer
 if (cardnum < 10) then
  case colornum of 
   0 -> return $ RegularCard cardnum Blue
   1 -> return $ RegularCard cardnum Red
   2 -> return $ RegularCard cardnum Yellow
   3 -> return $ RegularCard cardnum Green
 else
  case (cardnum, colornum) of
   (10, 0) -> return $ SpecialCard Pick2 Blue
   (10, 1) -> return $ SpecialCard Pick2 Red
   (10, 2) -> return $ SpecialCard Pick2 Yellow
   (10, 3) -> return $ SpecialCard Pick2 Green
   (11, 0) -> return $ SpecialCard Pick4 Blue
   (11, 1) -> return $ SpecialCard Pick4 Red
   (11, 2) -> return $ SpecialCard Pick4 Yellow
   (11, 3) -> return $ SpecialCard Pick4 Green
   (12, 0) -> return $ SpecialCard Switch Blue
   (12, 1) -> return $ SpecialCard Switch Red
   (12, 2) -> return $ SpecialCard Switch Yellow
   (12, 3) -> return $ SpecialCard Switch Green
   (13, 0) -> return $ SpecialCard Skip Blue
   (13, 1) -> return $ SpecialCard Skip Red
   (13, 2) -> return $ SpecialCard Skip Yellow
   (13, 3) -> return $ SpecialCard Skip Green