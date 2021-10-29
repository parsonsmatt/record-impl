{-# language TemplateHaskell, OverloadedRecordDot #-}

import Impl.TH

impl ''User [d|
    explode :: IO ()
    explode = do
        user.myPrint 'a'
    |]

main :: IO ()
main = do
    let user = User { name = "Matt" }
    user.myPrint 'a'
