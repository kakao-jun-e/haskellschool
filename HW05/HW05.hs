{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module HW05 where

import Data.ByteString.Lazy (ByteString)
import Data.Bits (xor)
import Data.Map.Strict (Map)
import System.Environment (getArgs)

import qualified Data.ByteString.Lazy as BS
import qualified Data.Map.Strict as Map
import qualified Data.List as List

import Parser

-- Exercise 1 -----------------------------------------

getSecret :: FilePath -> FilePath -> IO ByteString
getSecret encoded_path original_path = do
  encoded_image <- BS.readFile encoded_path
  original_image <- BS.readFile original_path
  return $ BS.filter (/= 0) $ BS.pack $ BS.zipWith (xor) encoded_image original_image

-- Exercise 2 -----------------------------------------

decryptWithKey :: ByteString -> FilePath -> IO ()
decryptWithKey encryption_key file_path = do
  encrypted_json <- BS.readFile (file_path ++ ".enc")
  BS.writeFile file_path $ BS.pack $ BS.zipWith (xor) (BS.cycle encryption_key) encrypted_json

-- Exercise 3 -----------------------------------------

parseFile :: FromJSON a => FilePath -> IO (Maybe a)
parseFile json_path = do
  transactions <- BS.readFile json_path
  return $ decode transactions

-- Exercise 4 -----------------------------------------

getBadTs :: FilePath -> FilePath -> IO (Maybe [Transaction])
getBadTs victims_path transaction_path = do
  victims_tid <- (parseFile victims_path) :: IO (Maybe [TId])
  transactions <- (parseFile transaction_path) :: IO (Maybe [Transaction])
  return $ (\vs ts -> filter (\z -> elem (tid z) vs) ts) <$> victims_tid <*> transactions

-- Exercise 5 -----------------------------------------

getFlow :: [Transaction] -> Map String Integer
getFlow = foldr foldFlow Map.empty
  where foldFlow (Transaction from to amount _) = Map.insert to amount . Map.insert from (negate amount)

-- Exercise 6 -----------------------------------------

getCriminal :: Map String Integer -> String
getCriminal = fst . foldr1 (\max_value value -> if (snd max_value) > (snd value) then max_value else value) . Map.toList

-- Exercise 7 -----------------------------------------

undoTs :: Map String Integer -> [TId] -> [Transaction]
undoTs flow_map tid = zipWith (\id (from, to, amount) -> Transaction from to amount id) tid $ repay (getPayers flow_map) (getPayees flow_map)
  where repay :: [(String, Integer)] -> [(String, Integer)] -> [(String, String, Integer)]
        repay [] _ = []
        repay _ [] = []
        repay (payer:payers) (payee:payees)
          | minimum_amount == (snd payer) = (from_name, to_name, minimum_amount) : repay payers ((to_name, to_amount):payees)
          | minimum_amount == (snd payee) = (from_name, to_name, minimum_amount) : repay ((from_name, from_amount):payers) payees
          | otherwise = repay payers payees
          where minimum_amount = min (snd payer) ((negate . snd) payee)
                from_name = fst payer
                from_amount = (snd payer) - minimum_amount
                to_name = fst payee
                to_amount = (snd payee) + minimum_amount
        getPayers = List.sortBy descendingOrder . Map.toList . Map.filter (>0)
        getPayees = reverse . List.sortBy descendingOrder . Map.toList . Map.filter (<0)
        descendingOrder (_, x) (_, y) = compare y x

-- Exercise 8 -----------------------------------------

writeJSON :: ToJSON a => FilePath -> a -> IO ()
writeJSON file_path = BS.writeFile file_path . encode

-- Exercise 9 -----------------------------------------

doEverything :: FilePath -> FilePath -> FilePath -> FilePath -> FilePath
             -> FilePath -> IO String
doEverything dog1 dog2 trans vict fids out = do
  key <- getSecret dog1 dog2
  decryptWithKey key vict
  mts <- getBadTs vict trans
  case mts of
    Nothing -> error "No Transactions"
    Just ts -> do
      mids <- parseFile fids
      case mids of
        Nothing  -> error "No ids"
        Just ids -> do
          let flow = getFlow ts       
          writeJSON out (undoTs flow ids)
          return (getCriminal flow)

main :: IO ()
main = do
  args <- getArgs
  crim <- 
    case args of
      dog1:dog2:trans:vict:ids:out:_ ->
          doEverything dog1 dog2 trans vict ids out
      _ -> doEverything "dog-original.jpg"
                        "dog.jpg"
                        "transactions.json"
                        "victims.json"
                        "new-ids.json"
                        "new-transactions.json"
  putStrLn crim

