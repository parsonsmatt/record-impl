{-# language GADTs, UndecidableInstances, TemplateHaskell, OverloadedRecordDot, DataKinds #-}

{-# OPTIONS_GHC -ddump-splices #-}

module Impl where

-- -- $> :set -XFlexibleContexts
--
-- import Impl.TH
-- import GHC.Records
--
-- impl ''User [d|
--     greet :: String -> IO ()
--     greet message = do
--         putStrLn $ concat [ message, ", ", "asdf" ]
--     |]
--
--
-- uhhWhat :: IO ()
-- uhhWhat = (User { name = "asdf", age = 32}).explode
--
-- -- $> let user = User { name = "Matt parsons", age = 32 }
--
-- -- $> user.sayName
--
-- -- user.explode
--
-- asdf :: Int
-- asdf = 10
--
-- -- $> asd
