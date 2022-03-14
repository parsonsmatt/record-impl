{-# language QuasiQuotes #-}
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
        f self.userName

    sayName :: IO ()
    sayName =
        putStrLn self.userName

    |]

runExample :: IO ()
runExample = do
    let user = User { userName = "Matt", userBirthYear = 33 }

    user.sayName
