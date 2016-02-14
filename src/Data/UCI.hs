{-# LANGUAGE FlexibleInstances #-}

module Data.UCI where

import Prelude hiding (Read, Show, read, show)

data MessageIn = UCI
               | Debug (Maybe Bool)
               | IsReady
               | NewGame
               | Position (Maybe String) [String]
               | Go
               | Quit
               | Unknown [String]

data MessageOut = ID String String
                | OK
                | ReadyOK
                | BestMove String (Maybe String)
                | Info String

class Read a where read :: a -> [MessageIn]
class Show a where show :: MessageOut -> [a]

instance Read [String] where
    read ["uci"]                                            = [UCI]
    read ["debug"]                                          = [Debug  Nothing]
    read ["debug", "on"]                                    = [Debug (Just True)]
    read ["debug", "off"]                                   = [Debug (Just False)]
    read ["isready"]                                        = [IsReady]
    read ["ucinewgame"]                                     = [NewGame]
    read ("position":"fen":placement:_:_:_:_:_ :"moves":ws) = [Position undefined ws]
    read ("position":"startpos"                :"moves":ws) = [Position undefined ws]
    read ("position"                           :"moves":ws) = [Position undefined ws]
    read ("go":_)                                           = [Go]
    read ws                                                 = [Unknown ws]

instance Show [String] where
    show (ID name author)        = [["id", "name", name], ["id", "author", author]]
    show  OK                     = [["uciok"]]
    show  ReadyOK                = [["readyok"]]
    show (BestMove m1 Nothing)   = [["bestmove", m1]]
    show (BestMove m1 (Just m2)) = [["bestmove", m1, "ponder", m2]]
    show (Info xs)               = [["info", "string", xs]]

instance Read String where read = read . words
instance Show String where show m = [unwords (show m)]
