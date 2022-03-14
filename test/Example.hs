{-# language DataKinds, QuasiQuotes, OverloadedRecordDot #-}
{-# language TemplateHaskell #-}

module Example where

import Impl.TH

data User = User
    { name :: String
    , birthYear :: Int
    }

impl ''User [d|

    applyName :: (String -> String) -> String
    applyName f =
        f self.name

    sayName :: IO ()
    sayName =
        putStrLn self.name

    |]

runExample :: IO ()
runExample = do
    let user = User { name = "Matt", birthYear = 33 }

    user.sayName
