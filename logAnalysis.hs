{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log

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

