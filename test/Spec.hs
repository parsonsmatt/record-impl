{-# language TemplateHaskell, OverloadedRecordDot, DataKinds, OverloadedRecordDot  #-}

import Impl.TH
import Example

main :: IO ()
main = do
    let user = User { name = "Matt", birthYear = 1988 }
    user.sayName
