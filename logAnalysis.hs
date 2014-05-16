{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log
-- Helper functions
--
-- | Given a LogMessage returns the timestamp
-- >>> timeStamp (LogMessage Info 2 "x")
-- 2
--
-- >>> timeStamp (Unknown "foo")
-- 0
--
timeStamp :: LogMessage -> TimeStamp
timeStamp (LogMessage _ ts _) = ts
timeStamp (Unknown _) = 0

-- | Given a LogMessage returns the severity
-- >>> severity (LogMessage (Error 2) 3 "x")
-- 2
--
-- >>> severity (LogMessage Info 2 "foo")
-- 0
--
severity :: LogMessage -> Int
severity (LogMessage (Error s) _ _) = s
severity _ = 0

-- | Given a LogMessage tells whether it's an Error or not
-- >>> isError (LogMessage (Error 2) 3 "x")
-- True
--
-- >>> isError (LogMessage Info 2 "foo")
-- False
--
isError :: LogMessage -> Bool
isError (LogMessage (Error _) _ _) = True
isError _ = False

-- | Given an LogMessage returns the description
-- >>> description (LogMessage (Error 2) 3 "x")
-- "x"
--
-- >>> description (LogMessage Info 2 "foo")
-- "foo"
--
-- >>> description (Unknown "bar")
-- "bar"
--
description :: LogMessage -> String
description (LogMessage _ _ message) = message
description (Unknown message) = message

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

-- | Builds an in order list of LogMessages from a MessageTree
--
-- >>> let foo = LogMessage Warning 10 "foo"
-- >>> let baz = LogMessage Warning 5 "baz"
-- >>> let bif = LogMessage Warning 15 "bif"
-- >>> let gaz = LogMessage (Error 1) 20 "gaz"
-- >>> let tree = Node (Node Leaf baz Leaf) foo (Node Leaf bif (Node Leaf gaz Leaf))
-- >>> inOrder tree == [baz, foo, bif, gaz]
-- True
--
inOrder :: MessageTree -> [LogMessage]
inOrder (Node left root right) = inOrder left ++ [root] ++ inOrder right
inOrder Leaf = []

-- | Find messages with errors whose severity is 50+
--
-- >>> let messages = [LogMessage (Error 49) 10 "alpha", LogMessage (Error 100) 9 "kappa", LogMessage (Error 51) 11 "beta", Unknown "foo", LogMessage Warning 100 "blar"]
-- >>> whatWentWrong messages == ["kappa", "beta"]
-- True
--
whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = map description . (filter (\x -> (severity x) >= 50)) . (filter isError) . inOrder . build
