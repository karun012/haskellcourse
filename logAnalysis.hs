{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log
-- Helper functions
--
-- >>> timeStamp (Info 2 "x")
-- 2
--
-- >>> timeStamp (Unknown "foo")
-- 0
--
timeStamp :: LogMessage -> TimeStamp
timeStamp (LogMessage _ ts _) = ts
timeStamp _ = 0

-- | Parse a string into a LogMessage
--
-- >>> parseMessage "E 2 562 help help"
-- LogMessage (Error 2) 562 "help help"
--
-- >>> parseMessage "I 29 la la la"
-- LogMessage Info 29 "la la la"
--
-- >>> parseMessage "W 29 whatever wat wat"
-- LogMessage Warning 29 "whatever wat wat"
--
-- >>> parseMessage "This is not in the right format"
-- Unknown "This is not in the right format"
--
parseMessage :: String -> LogMessage
parseMessage xs = case words xs of 
                  ("I":ts:message) -> LogMessage Info (read ts) (unwords message)
                  ("E":code:ts:message) -> LogMessage (Error (read code)) (read ts) (unwords message)
                  ("W":ts:message) -> LogMessage Warning (read ts) (unwords message)
                  (_) -> Unknown xs

-- | Parse a file into a list of LogMessages
--
-- >>> testParse parse 1 "sample.log"
-- [LogMessage Info 6 "Completed armadillo processing"]
--
parse :: String -> [LogMessage]
parse = (map parseMessage) . lines


-- | Inserts a LogMessage into a MessageTree
--
-- >>> insert (Unknown "foo") Leaf
-- Leaf
--
-- >>> insert (parseMessage "I 2 x") Leaf
-- Node Leaf (LogMessage Info 2 "x") Leaf
--
-- insert (LogMessage Info 5 "baz") Leaf
-- Node Leaf (LogMessage Info 5 "baz") Leaf
-- 
-- >>> let foo = LogMessage Info 10 "foo"
-- >>> let baz = LogMessage Info 5 "baz"
-- >>> let bif = LogMessage Info 15 "bif"
-- >>> let a = Node Leaf foo Leaf
--
-- >>> let b = insert baz a
-- >>> b == Node (Node Leaf baz Leaf) foo Leaf
-- True
--
-- >>> let c = insert bif b
-- >>> c == Node (Node Leaf baz Leaf) foo (Node Leaf bif Leaf)
-- True
--
insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert logMessage@(_) Leaf = Node Leaf logMessage Leaf
insert logMessage (Node left lm right) = case timeStamp logMessage < timeStamp lm of
                                         True -> Node (insert logMessage left) lm right
                                         False -> Node left lm (insert logMessage right)

-- | Builds a MessageTree from a list of messages
--
-- >>> let foo = LogMessage Info 10 "foo"
-- >>> let baz = LogMessage Info 5 "baz"
-- >>> let bif = LogMessage Info 15 "bif"
-- 
-- >>> let result = build [foo, baz, bif]
-- >>> result == Node (Node Leaf baz Leaf) foo (Node Leaf bif Leaf)
-- True
--
build :: [LogMessage] -> MessageTree
build = foldl (flip insert) Leaf
