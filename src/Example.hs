{-# language QuasiQuotes #-}
{-# language TemplateHaskell #-}

module Example where

import Impl.TH

impl ''User [d|

    applyName :: (String -> String) -> String
    applyName f =
        f self.name

    sayName :: _
    sayName =
        putStrLn self.name

    |]

runExample :: IO ()
runExample = do
    let user = User { userName = "Matt", userBirthYear = 33 }

    user.sayName
