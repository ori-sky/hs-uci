{-# LANGUAGE FlexibleInstances #-}

module Data.UCI where

data MessageIn = UCI
               | Debug (Maybe Bool)
               | IsReady
               | NewGame
               | Position (Maybe String) [String]
               | Go
               | Quit
                 deriving Show

data MessageOut = ID String String
                | OK
                | ReadyOK
                | BestMove String (Maybe String)
                | Info String
                  deriving Show

class ReadIn a  where readIn  :: a -> [MessageIn]
class ShowOut a where showOut :: MessageOut -> [a]

instance ReadIn [String] where
    readIn ["uci"]                                            = [UCI]
    readIn ["debug"]                                          = [Debug  Nothing]
    readIn ["debug", "on"]                                    = [Debug (Just True)]
    readIn ["debug", "off"]                                   = [Debug (Just False)]
    readIn ["isready"]                                        = [IsReady]
    readIn ["ucinewgame"]                                     = [NewGame]
    readIn ("position":"fen":placement:_:_:_:_:_ :"moves":ws) = [Position undefined ws]
    readIn ("position":"startpos"                :"moves":ws) = [Position undefined ws]
    readIn ("position"                           :"moves":ws) = [Position undefined ws]
    readIn ("go":_)                                           = [Go]
    readIn ws                                                 = []

instance ShowOut [String] where
    showOut (ID name author)        = [["id", "name", name], ["id", "author", author]]
    showOut  OK                     = [["uciok"]]
    showOut  ReadyOK                = [["readyok"]]
    showOut (BestMove m1 Nothing)   = [["bestmove", m1]]
    showOut (BestMove m1 (Just m2)) = [["bestmove", m1, "ponder", m2]]
    showOut (Info xs)               = [["info", "string", xs]]

instance ReadIn String where readIn = readIn . words
instance ShowOut String where showOut m = [unwords (showOut m)]
