{-# language QuasiQuotes #-}
{-# language TemplateHaskell #-}

{-# options_ghc -fno-warn-orphans #-}

module Example where

import Impl.TH

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
