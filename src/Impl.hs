{-# language GADTs, UndecidableInstances, TemplateHaskell, OverloadedRecordDot, DataKinds #-}

module Impl where

import Control.Monad.IO.Class
import Impl.TH

data User = User
    { name :: String
    , age :: Int
    }
    deriving Show

impl ''User [d|
    greet :: String -> IO ()
    greet message = do
        putStrLn $ concat [ message, ", ", self.name ]

    explode :: IO ()
    explode =
        error "oh no"

    vote :: IO ()
    vote =
        if self.age >= 18
            then putStrLn "voting"
            else putStrLn "alas"
    |]


uhhWhat :: IO ()
uhhWhat = user.explode

user :: User
user = (User { name = "asdf", age = 32})

-- $> :load Impl
--
-- $> user.greet "hello"
--
-- $> user.vote
