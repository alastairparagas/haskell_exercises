module LogAnalysis where

import Log

{-
  Define a function
    parseMessage :: String -> LogMessage
  which parses an individual line from the log file.
  
  Define a function
    parse :: String -> [LogMessage]
  which parses an entire log file at once and returns 
  its contents as a list of LogMessages.
-}
parseMessage :: String -> LogMessage
parseMessage logFileMessage = case (words logFileMessage) of 
  "I" : timeStamp : stringMessage -> 
    LogMessage Info (read timeStamp) (unwords stringMessage)
  "W" : timeStamp : stringMessage -> 
    LogMessage Warning (read timeStamp) (unwords stringMessage)
  "E" : errorCode : timeStamp : stringMessage -> 
    let arg1 = Error (read errorCode)
        arg2 = read timeStamp
        arg3 = unwords stringMessage 
    in LogMessage arg1 arg2 arg3
  stringMessage -> 
    Unknown (unwords stringMessage)

parse :: String -> [LogMessage]
parse linesInFile = case (lines linesInFile) of 
  []  -> []
  xs  -> map parseMessage xs

{-
  Define a function
    insert :: LogMessage -> MessageTree -> MessageTree
  which inserts a new LogMessage into an existing MessageTree, 
  producing a new MessageTree. insert may assume that it is 
  given a sorted MessageTree, and must produce a new sorted 
  MessageTree containing the new LogMessage in addition to the 
  contents of the original MessageTree.
  
  However, note that if insert is given a LogMessage which is 
  Unknown, it should return the MessageTree unchanged.
-}
insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) sortedMessageTree = sortedMessageTree
insert logMsg Leaf                   = Node Leaf logMsg Leaf
insert 
  logMsg@(LogMessage _ timestamp _) 
  (Node leftMsgTree currentLogMsg@(LogMessage _ currentTimestamp _) rightMsgTree) =
    if timestamp <= currentTimestamp then
      Node (insert logMsg leftMsgTree) currentLogMsg rightMsgTree
    else
      Node leftMsgTree currentLogMsg (insert logMsg rightMsgTree)

{-
  Define a function
    build :: [LogMessage] -> MessageTree
  which builds up a MessageTree containing the messages in the list, 
  by successively inserting the messages into a MessageTree 
  (beginning with a Leaf).
-}
build :: [LogMessage] -> MessageTree
build []      = Leaf
build (x:xs)  = insert x (build xs)

{-
  Define the function 
    inOrder :: MessageTree -> [LogMessage]
  which takes a sorted MessageTree and produces a list of all the 
  LogMessages it contains, sorted by timestamp from smallest to biggest. 
  (This is known as an in-order traversal of the MessageTree.)
  With these functions, we can now remove Unknown messages and sort the 
  well-formed messages using an expression such as:
    inOrder (build tree)
-}
inorder :: MessageTree -> [LogMessage]
inorder Leaf                                = []
inorder (Node Leaf logMessage messageTree)  = 
  logMessage : inorder messageTree 
inorder (Node leftMessageTree logMessage rightMessageTree) = 
  (inorder leftMessageTree) ++ 
  [logMessage] ++ 
  (inorder rightMessageTree)
  
{-
  Write a function
    whatWentWrong :: [LogMessage] -> [String]
  which takes an unsorted list of LogMessages, and returns a 
  list of the messages corresponding to any errors with a 
  severity of 50 or greater, sorted by timestamp. (Of course, 
  you can use your functions from the previous exercises to do 
  the sorting.)
-}
whatWentWrong :: [LogMessage] -> [String]
whatWentWrong [] = []
whatWentWrong xs = 
  let filterErrorLogMessage logMessage = case logMessage of 
        LogMessage (Error errorLevel) _ _  -> errorLevel >= 50
        otherwise                          -> False
      mapErrorLogMessageToString (LogMessage _ _ message) = 
        message
  in map mapErrorLogMessageToString 
    (inorder (build (filter filterErrorLogMessage xs)))
