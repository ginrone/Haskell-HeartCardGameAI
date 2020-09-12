-- | Name : Sulthan De Neiro Raihan Syah Bungin
-- | ID : 29906164
-- | Assignment 2 Programming Paradigm Semester 2 2019
--
-- Strategy for playing Hearts
-- 
-- Strategy divided into 2 parts, one for lead and one for renege
-- 
-- Lead Strategy:
-- 
-- General Idea : 
-- a. AI will 100% play Card Club Two at the beginning of the game (if available )
-- b. Avoid Leading Spade Queen (This is not a shoot to the moon strategy)
-- c. Most of the time try to lead with lower cards (Safe play)
-- d. Try to Force other player to play Spade Queen by spamming high spades for lead
-- e. When in non-broken-heart state, do not play heart (The rule)
--
-- The AI will prioritize the strategy in ascending order (1,2,3,...)
-- Lead has 4 different conditions and each has different 4 strategies
--
--     a. When the leader does not hold SQ and the heart has not broken
--         1. AI will choose the lowest card that is not heart (since heart has not broken yet) to avoid the future lead
--         2. AI will choose the lowest card in hand (mostly hearts because there are no more options) 
--     b. When the leader does not hold SQ and has been broken
--         1. AI will choose the highest spade that rank is less than Queen
                --This method is to force other players to dump SQ while gain high chance of leading the turn
--         2. AI will choose the lowest card that is less than Queen(Not playing J Q K A) to avoid future lead and SQ from other players
--         3. AI will choose lowest card in hand (mostly J,Q,K,A because there are no more options)
--     c. When the leader does hold SQ and the heart has not broken
--         1. AI will choose the lowest non-Spade (to avoid being forced to dump SQ by itself) and non-Heart (Since heart has not broken yet)
--         2. AI will choose the lowest Spade (except Spade Queen) to avoid future lead
--         3. AI will choose the lowest non-heart (Since heart has not been broken) to avoid future lead
--         4. AI will choose lowest card in hand (mostly SQ, Hearts, which would not be likely to be played)
--     d. When the leader does hold SQ and the heart has been broken
--         1. AI will choose the lowest heart to safely lead with heart and speed up the game also prevent future lead
--         2. AI will choose the lowest card (except Spade Queen) to avoid future lead
--         3. AI will choose the lowest card in hand (the remaining Spade Queen)
--
-- Renege Strategy:
--
-- General Idea:
-- a. Play card that is lower than the leader if in the same suit (except first turn)
-- b. Play points card if there are no cards in hand that has the same suit as the leader (Prioritize Spade Queen for aggresive play)
-- c. Try to avoid future lead for most of the time (To safely avoid points in the late game)  
--
-- The AI will prioritize the strategy in ascending order (1,2,3,...)
-- There are 2 cases for renege:
--     a. When the leader is Card Club Two
--          1. AI will choose the highest Club in hand to preserved the low cards while having lead advantage (since its 100% impossible to play point cards in the first round)
--          2. AI will choose the highest non-points card to preserved the low cards while having lead advantage
--          3. AI will choose the highest card in hand (mostly would be points card, but this case is very rare to occur)
--     b. When the leader is not Card Club Two
--          1. AI will choose the highest card with the same suit in hand with the rank that is less than the leader
--          2. AI will choose the lowest card with the same suit in hand to lower the chance of having lead next turn (except Card Spade Queen)
--          3. AI will choose Card Spade Queen if there are no card within the same suit (Aggresive play to make the other players take points)
--          4. AI will choose the highest heart to preserved low cards for the future while also making other players to take points
--          5. AI will choose the highest card in hand (the rest of the cards, most likely would be non-points cards)

module Player (
    playCard,makeBid,lead,renege,sort,theSort
)
where

import Cards
import Data.Maybe
import Data.List
import Hearts.Types

-- | Function to play the game
-- returns nothing
-- Either the player lead or renege and cannot do both things at the same time
-- param: hand is the list of cards in hand
-- param: trick is the current trick
-- param: prevTrick is the previous trick
playCard :: PlayFunc
playCard _ hand [] prevTrick = (lead hand (readMemory prevTrick), writeMemory prevTrick)
playCard _ hand trick prevTrick = (renege (fst $ last trick) hand, writeMemory prevTrick)

-- | Function to convert Card into list of cards
-- return a list of cards (thats only 1 card)
-- Will be use to detect the chosen card by the leaders later on
-- param: Card is the card that will be converted into list
toList :: Card -> [Card]
toList (Card theSuit theRank) = [(Card theSuit theRank)] 

-- | Function to write the memory
-- return a string 
-- A function that write the memory of a previous trick and store it in a form of string
-- param: Nothing means the history is empty
-- param: aMemory is the single previous memory
-- param: entireMemory is the entire memory in the string
writeMemory :: Maybe ([(Card, PlayerId)], String) -> String
writeMemory Nothing = ""
writeMemory (Just (aMemory,entireMemory)) = entireMemory ++ show(aMemory)

-- | Function to read the memory
-- return a list of cards
-- A function that read the memory of the previous tricks which will be used to detect whether or not heart is broken
-- param: aMemory is the single previous memory
readMemory :: Maybe ([(Card, PlayerId)], String) -> [Card]
readMemory Nothing = []
readMemory (Just (aMemory,_)) = fst <$> aMemory

-- | Function to select the suit of the card
-- return the suit
-- param: the Card 
suit :: Card -> Suit
suit (Card theSuit _) = theSuit

-- | Function to select the rank of the card
-- return the rank
-- param: the Card
rank :: Card -> Rank
rank (Card _ theRank) = theRank

-- | Function sort the list of cards
-- return list of cards
-- A sort function that Prioritizing the card's rank instead of card's suit (S2,C2,S3,...) rather than (S2,S3,C2,...)
-- Will be used for sorting list of cards with suit more than 1
-- The filters are needed to be hardcoded due to specify the sorting based on the card's rank first rather than the card's suit
-- param: hand is the list of cards
theSort :: [Card] -> [Card]
theSort hand = ((filter (\x -> rank x == Two) hand) ++
                (filter (\x -> rank x == Three) hand) ++
                (filter (\x -> rank x == Four) hand) ++
                (filter (\x -> rank x == Five) hand) ++
                (filter (\x -> rank x == Six) hand) ++
                (filter (\x -> rank x == Seven) hand) ++
                (filter (\x -> rank x == Eight) hand) ++
                (filter (\x -> rank x == Nine) hand) ++
                (filter (\x -> rank x == Ten) hand) ++
                (filter (\x -> rank x == Jack) hand) ++
                (filter (\x -> rank x == Queen) hand) ++
                (filter (\x -> rank x == King) hand) ++
                (filter (\x -> rank x == Ace) hand))
                
-- | Not used, and cannot be remove
makeBid :: BidFunc
makeBid = undefined

-- | Function to renege when a lead card has been played
-- return a card 
-- The renege strategy explained in the above
-- param: leader is the card that given by the leader of the trick
-- param: hand is the list of cards of the players
renege :: Card -> [Card] -> Card
renege leader hand = check (filter (\x -> x == Card Club Two) (toList (leader))) where
    -- | Function to check the first turn or not
    -- return a card
    -- Checking the turn to determine the player strategy
    -- param: the leader card         
    check :: [Card] -> Card
    -- If its not the first turn
    check [] = head $ ((reverse . sort $ ((filter (\x -> suit x == suit leader && rank x < rank leader)) hand)) ++  
                        (sort $ ((filter (\x -> suit x == suit leader && x /= Card Spade Queen)) hand)) ++
                        ((filter (\x -> x == Card Spade Queen)) hand) ++ 
                        (reverse . sort $ (filter (\x -> suit x == Heart)) hand) ++
                        (reverse . theSort $ hand))
    -- If its the first turn
    check _ = head $ ((reverse . sort $ (filter (\x -> suit x == Club) hand)) ++
                        (reverse . theSort $ ((filter (\x -> suit x /= Heart && x /= Card Spade Queen)) hand)) ++
                        (reverse . theSort $ hand))

-- | Function to lead the trick
-- return a card
-- The first lead will and always be Card Club Two
-- The lead would be determined with certain strategies that has been explained above
-- param: hand is the list of cards of the player's hand
-- param: memory is the history of the previous trick
lead :: [Card] -> [Card] -> Card 
lead _ [] = (Card Club Two) 
lead hand memory = check (filter (\x -> x == Card Spade Queen) hand) (filter (\x -> suit x == Heart) memory) where
        -- | Function to check the Spade Queen being hold and check the heart has been broken or not
        -- return the card
        -- param: the list of hand 
        -- param: the memory
        check :: [Card] -> [Card] -> Card
        -- When not holding Spade Queen
            -- When heart is not broken
                            --AI will choose the lowest card that is not heart (since heart has not broken yet) to avoid the future lead
        check [] [] = head $ ((filter (\x -> suit x /= Heart) . theSort $ hand) ++ 
                            --AI will choose the lowest card in hand (mostly hearts because there are no more options)
                                (theSort $ hand))
            -- When heart is broken
                            --AI will choose the highest spade that rank is less than Queen
        check [] _ = head $ ((reverse . sort $ (filter (\x -> suit x == Spade && rank x < Queen)) hand) ++ 
                            --AI will choose the lowest card that is less than Queen(Not playing J Q K A) to avoid future lead and SQ from other players
                                (theSort $ (filter (\x -> rank x < Queen)) hand) ++
                            --AI will choose lowest card in hand (mostly J,Q,K,A because there are no more options)
                                (theSort hand))
        -- When holding Spade Queen
            -- When heart is not broken
                            --AI will choose the lowest non-Spade (to avoid being forced to dump SQ by itself) and non-Heart (Since heart has not broken yet)
        check _ [] = head $ ((theSort $ (filter (\x -> suit x /= Spade && suit x /= Heart)) hand) ++
                            --AI will choose the lowest Spade (except Spade Queen) to avoid future lead
                                (sort $ (filter (\x -> suit x == Spade && x /= Card Spade Queen)) hand) ++
                            --AI will choose the lowest non-heart (Since heart has not been broken) to avoid future lead
                                (theSort $ (filter (\x -> suit x /= Heart)) hand) ++
                            --AI will choose lowest card in hand (mostly SQ, Hearts, which would not be likely to be played)
                                (theSort hand))
            -- When heart is broken
                            --AI will choose the lowest heart to safely lead with heart and speed up the game also prevent future lead
        check _ _ = head $ ((sort $ (filter (\x -> suit x == Heart)) hand) ++
                            --AI will choose the lowest card (except Spade Queen) to avoid future lead
                                (theSort $ (filter (\x -> x /= Card Spade Queen)) hand) ++
                            --AI will choose the lowest card in hand (the remaining Spade Queen)
                                (theSort hand))