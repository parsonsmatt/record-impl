{-# language LambdaCase, EmptyCase, TypeApplications, RankNTypes, TypeFamilyDependencies, TypeFamilies, GADTs, ScopedTypeVariables, UndecidableInstances, ImpredicativeTypes, OverloadedRecordDot, QuasiQuotes, NoMonomorphismRestriction, DataKinds, FlexibleContexts , TemplateHaskell #-}

{-# language RankNTypes, FlexibleInstances, MultiParamTypeClasses, FunctionalDependencies, ExistentialQuantification, ExplicitForAll, ImpredicativeTypes, RankNTypes #-}

{-# Language RebindableSyntax, OverloadedRecordDot, GADTs, TemplateHaskell, ImpredicativeTypes, FlexibleContexts, UndecidableInstances, FlexibleInstances #-}

module Impl.TH where

import Data.Map (Map)
import qualified Data.Map as Map
import Prelude hiding (exp)
import Data.Time
import GHC.Records
import Language.Haskell.TH
-- import Language.Haskell.TH.Syntax

giveIdentity :: IO (forall a . Show a => a -> String)
giveIdentity = pure show

data User = User
    { userName :: String
    , userBirthYear :: Integer
    }
    deriving Show

instance HasField "setName" User (String -> User) where
    getField self str = self { userName = str }

instance
    ( Show a
    , HasField "myPrint" User (a -> IO ())
    )
  =>
    HasField "myPrint" User (a -> IO ())
  where
    getField self a = do
        putStrLn $ concat [self.userName, " says: ", show a]

instance HasField "age" User (IO Integer) where
    getField self = do
        (y, _, _) <- toGregorian . utctDay <$> getCurrentTime
        pure $ y - self.userBirthYear


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
                getField _self = do
                    putStrLn "oh no"
            |]
    selfName <- newName "self"
    realDecs <- map (fixDecs typ selfName) <$> decsQ
    _ <- traverse (reportWarning . show)  $ realDecs
    pure decs

collectDecs :: [Dec] -> Map Name [Dec]
collectDecs [] = mempty
collectDecs (d : ds) = Map.unionWith mappend go $ collectDecs ds
  where
    go =
        case d of
            ValD pat _ _ ->
                Map.singleton (getName pat) [d]
            FunD name _ ->
                Map.singleton name [d]
            SigD name _ ->
                Map.singleton name [d]
            KiSigD name _ ->
                Map.singleton name [d]
            _ ->
                error $ "unsupported dec: " <> show d
    getName p = case p of
        VarP name ->
            name
        ConP {} ->
            error "constructor not supported"
        LitP {} ->
            no
        TupP {} -> no
        UnboxedTupP {} -> no
        InfixP {} -> no
        UnboxedSumP {} -> no
        UInfixP {} -> no
        ParensP {} -> no
        TildeP {} -> no
        BangP {} -> no
        _ -> no

      where
        no = error $ "unsupported pattern: " <> show p



fixDecs :: Name -> Name -> Dec -> Dec
fixDecs typeName selfName d = go
  where

    replaceSelf :: Exp -> Exp
    replaceSelf x = case x of
        UnboundVarE nm
            | nm == mkName "self" ->
                error "asdfasdfdasdf"
        VarE _ ->
            x
        ConE _ ->
            x
        _ ->
            x

    mkLambda :: Exp -> Exp
    mkLambda = LamE [SigP (VarP selfName) (ConT typeName)]

    addSelfGuardExps :: (Guard, Exp) -> (Guard, Exp)
    addSelfGuardExps (grd, exp) = (newGrd, newExp)
      where
        newExp =
            case exp of
                VarE {} ->
                    exp
                ConE {} ->
                    exp
                LitE {} ->
                    exp
                AppE e0 e1 ->
                    mkLambda $ AppE (replaceSelf e0) (replaceSelf e1)
                AppTypeE e0 t ->
                    AppTypeE (mkLambda e0) t
                InfixE e0 e1 e2 ->
                    mkLambda $ InfixE (replaceSelf <$> e0) (replaceSelf e1) (replaceSelf <$> e2)
                UInfixE e0 e1 e2 ->
                    mkLambda $ UInfixE (replaceSelf e0) (replaceSelf e1) (replaceSelf e2)
                ParensE e ->
                    ParensE $ mkLambda $ replaceSelf e
                LamE a b ->
                    mkLambda $ LamE a (replaceSelf b)
                LamCaseE matches ->
                    mkLambda $ LamCaseE (map fixMatch matches)
                TupE mexps ->
                    mkLambda $ TupE (map (fmap replaceSelf) mexps)
                UnboxedTupE mexps ->
                    mkLambda $ UnboxedTupE (map (fmap replaceSelf) mexps)
                UnboxedSumE exp i j ->
                    mkLambda $ UnboxedSumE (replaceSelf exp) i j
                CondE a b c ->
                    mkLambda $ CondE (replaceSelf a) (replaceSelf b) (replaceSelf c)
                MultiIfE xs ->
                    mkLambda $ MultiIfE (map _f xs)
        newGrd =
            case grd of
                NormalG exp' ->
                    NormalG $ replaceSelf exp'

    fixMatch :: Match -> Match
    fixMatch = \case

    addSelfLambda :: Body -> Body
    addSelfLambda bdy =
        case bdy of
            GuardedB guard'exps ->
                GuardedB $ map addSelfGuardExps guard'exps
            NormalB exp ->
                NormalB $ LamE [SigP (VarP selfName) (ConT typeName)] exp


    addSelfClause :: Clause -> Clause
    addSelfClause (Clause patterns body decs) =
        Clause ((VarP selfName) : patterns) (addSelfLambda body) (map replaceSelfDecs decs)

    replaceSelfDecs :: Dec -> Dec
    replaceSelfDecs = \case


    addSelfType :: Type -> Type
    addSelfType t =
        ArrowT `AppT` ConT typeName `AppT` t

    go =
        case d of
            ValD pat body extras ->
                ValD pat (addSelfLambda body) extras
            FunD name clauses ->
                FunD name (map addSelfClause clauses)
            SigD name typ ->
                SigD name (addSelfType typ)
            KiSigD name typ ->
                KiSigD name (addSelfType typ)
            ForeignD _ ->
                error "foreign unsupported"
            InfixD fixity name ->
                InfixD fixity name

            -- it would be cool to support these
            PragmaD {} ->
                error "Pragma unsupported"
            DataFamilyD {} ->
                error "data family unsupported"
            DataInstD {} ->
                error "Data instance unsupported"
            NewtypeInstD {} ->
                error "newtype instance unsupported"
            TySynInstD {} ->
                error "type family instance unsupported"
            OpenTypeFamilyD {} ->
                error "type family declaration unsupported"
            ClosedTypeFamilyD {} ->
                error "close tyfam unsupported"
            RoleAnnotD {} ->
                error "role annotations unsupported"
            StandaloneDerivD {} ->
                error "standalone deriivng unsupported"
            DefaultSigD {} ->
                error "default unsupported"
            PatSynD {} ->
                error "pattern synonym unsupported"
            PatSynSigD {} ->
                error "pattern synonym unsupported"
            ImplicitParamBindD {} ->
                error "implicit param unsupported"
            DataD {} ->
                error "Data declarations unsupported"
            NewtypeD {} ->
                error "newtype unsupported"
            TySynD {} ->
                error "type synonym unsupported"
            ClassD {} ->
                error "classes unsupported"
            InstanceD {} ->
                error "instances unsupported"
