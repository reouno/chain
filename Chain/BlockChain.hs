{-# LANGUAGE OverloadedStrings #-}
module Chain.BlockChain (
    main
  , genesisBlock
  , Block (..)
  , Index
  , Hash256
  , Time
  , BlockData
  , calculateHash
  , generateNextBlock
  , isValidNewBlock
  , validateNewBlock
    ) where

import qualified Crypto.Hash.SHA256 as SHA256
import qualified Data.ByteString.Base16 as DB16
import qualified Data.ByteString as DB
import qualified Data.List as DL
import qualified Data.Text as DT
import qualified Data.Text.Encoding as DT
import qualified Data.UnixTime as DU

-- | An example function.
main :: IO ()
main = do
    putStrLn "main of BlockChain.hs"
    putStrLn "16 hash \"Hello, world.\":"
    print . DB16.encode . SHA256.hash  $ "Hello, world."
    currentT <- DU.getUnixTime
    print . show $ currentT
    print . DU.formatUnixTimeGMT DU.webDateFormat $ currentT
    print genesisBlock
    print $ calculateHash (index genesisBlock, previousHash genesisBlock, timestamp genesisBlock, blockData genesisBlock)
    currentT' <- DU.getUnixTime
    let block2 = generateNextBlock genesisBlock "This is the new data." currentT'
    print block2
    currnetT'' <- DU.getUnixTime
    let block3 = generateNextBlock block2 "This is third block data." currnetT''
    print block3
    putStr "is valid block3 = "
    print $ isValidNewBlock block2 block3
    print $ validateNewBlock block2 block3

--bs1 = "hello" :: DB.ByteString
--bs2 = "world" :: DB.ByteString
genesisBlock ::  Block
genesisBlock = Block {
                       index = 0
                     , previousHash = "0"
                     , timestamp = DU.UnixTime 1 0
                     , blockData = "First block data"
                     , thisHash = "a25fb5f5c022a89cac86fa0938233943eee01456bfc37505d57a2041242ae01e"
                       }

data Block = Block {
    index :: Index
  , previousHash :: Hash256
  , timestamp :: Time
  , blockData :: BlockData
  , thisHash :: Hash256
} deriving (Show, Ord, Eq)

-- ブロック内のデータ
type Index = Integer
type Hash256 = DB.ByteString
type Time = DU.UnixTime
type BlockData = DB.ByteString


calculateHash :: (Index, Hash256, Time, BlockData) -> DB.ByteString
calculateHash (i, h, t, d) = SHA256.hash s
    where
        s = DL.foldl' DB.append "" l
        l = [ DT.encodeUtf8 . DT.pack . show $ i
            , h
            , DT.encodeUtf8 . DT.pack . show $ t
            , d]

generateNextBlock :: Block -> BlockData -> DU.UnixTime -> Block
generateNextBlock block newData currnetT =
    Block { index = thisIndex
          , previousHash = prevHash
          , timestamp = currnetT
          , blockData = newData
          , thisHash = calculateHash (thisIndex, prevHash, currnetT, newData)}
          where
              thisIndex = index block + 1
              prevHash = thisHash block

isValidNewBlock :: Block -> Block -> Bool
isValidNewBlock prev new =
    (index new == index prev + 1)
    && (previousHash new == thisHash prev)
    && (calculateHash (index new, previousHash new, timestamp new, blockData new) == thisHash new)

validateNewBlock :: Block -> Block -> Maybe Block
validateNewBlock prev new =
    validateNewIndex prev new
    >>= validatePrevHash prev
    >>= validateThisHash

validateNewIndex :: Block -> Block -> Maybe Block
validateNewIndex prev new
    | index new == index prev + 1 = Just new
    | otherwise                   = Nothing

validatePrevHash :: Block -> Block -> Maybe Block
validatePrevHash prev new
    | previousHash new == thisHash prev = Just new
    | otherwise                         = Nothing

validateThisHash :: Block -> Maybe Block
validateThisHash new
    | calculateHash (index new, previousHash new, timestamp new, blockData new) == thisHash new = Just new
    | otherwise                                                                                 = Nothing
