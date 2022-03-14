{-# language TypeApplications, RankNTypes, TypeFamilyDependencies, TypeFamilies, GADTs, ScopedTypeVariables, UndecidableInstances, ImpredicativeTypes, OverloadedRecordDot, QuasiQuotes, NoMonomorphismRestriction, DataKinds, FlexibleContexts , TemplateHaskell #-}

{-# language RankNTypes, FlexibleInstances, MultiParamTypeClasses, FunctionalDependencies, ExistentialQuantification, ExplicitForAll, ImpredicativeTypes, RankNTypes #-}

{-# Language RebindableSyntax, OverloadedRecordDot, GADTs, TemplateHaskell, ImpredicativeTypes, FlexibleContexts, UndecidableInstances, FlexibleInstances #-}

module Impl.TH where

import Prelude
import Data.Time
import Control.Monad.IO.Class
import GHC.Records
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Quote

giveIdentity :: IO (forall a . Show a => a -> String)
giveIdentity = pure show

data User = User
    { name :: String
    , birthYear :: Integer
    }
    deriving Show

-- $> go

go :: IO ()
go = do
    let user = User { name = "Matt", birthYear = 1988}
    user.myPrint 'a'
    user.myPrint 3
    print =<< user.age

instance HasField "setName" User (String -> User) where
    getField self str = self { name = str }

instance
    ( Show a
    , HasField "myPrint" User (a -> IO ())
    )
  =>
    HasField "myPrint" User (a -> IO ())
  where
    getField self a = do
        putStrLn $ concat [self.name, " says: ", show a]

instance HasField "age" User (IO Integer) where
    getField self = do
        (y, _, _) <- toGregorian . utctDay <$> getCurrentTime
        pure $ y - self.birthYear


x :: Quote w => Code w Integer
x = [|| 1 + 2 ||]

-- What are we going for? Well, let's make functions on records.

-- data User = User
--     { name :: String
--     , age :: Int
--     }
--
-- sayName :: User -> IO ()
-- sayName user = putStrLn user.name
--
-- -- $> :set -XOverloadedRecordDot
--
-- -- $> let user = User { name = "matt parsons", age = 32 }
--
-- -- $> sayName user
--
-- -- What type does GHC infer here?
-- -- _name :: _ => _
-- _name a = a.name
--
-- -- This line is commented out, but it's the answer that GHC will give you
-- -- for the above wildcards.
-- _name :: HasField "name" r a => r -> a
--
-- -- Huh, okay, So maybe we can implement our own instances?
--
-- type IOish a = forall m . MonadIO m => m a
--
-- instance (MonadIO m) => HasField "sayName" User (IOish ()) where
--     getField self = putStrLn self.name
--
-- -- $> user.sayName
-- -- hello, world
--
--
-- -- ok hell yeah, cool. this is giving me some THoughts and Ideas. imagine:
--
-- {-
--
-- impl ''User [d|
--     sayName :: IO ()
--     sayName =
--         putStrLn self.name
--     |]
--
-- -}
--
-- -- this form would get translated to:
-- --
-- -- instance HasField "sayName" User (IO ())
-- --     getField self =
-- --         putStrLn self.name
-- --
--
-- -- what about function arguments?
--
-- instance HasField "greet" User (String -> IO ()) where
--     getField self greeting =
--         liftIO $ putStrLn $ concat [greeting, ", ", self.name, "!"]
--
-- -- $> user.greet "okay buddy"
--
-- -- okay, cool, so to do this, i am going to need to trudge around in some
-- -- TemplateHaskell.
--
impl :: Name -> Q [Dec] -> Q [Dec]
impl typ decsQ = do
    decs <-
        [d|
            instance  HasField "explode" $(conT typ) (IO ()) where
                getField self = do
                    putStrLn "oh no"
            |]
    realDecs <- decsQ
    traverse (reportError . show)  $ realDecs
    pure decs
--
-- -- $> :t impl
