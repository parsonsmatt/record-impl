{-# language LambdaCase, EmptyCase, TypeApplications, RankNTypes, TypeFamilyDependencies, TypeFamilies, GADTs, ScopedTypeVariables, UndecidableInstances, ImpredicativeTypes, OverloadedRecordDot, QuasiQuotes, NoMonomorphismRestriction, DataKinds, FlexibleContexts , TemplateHaskell #-}
{-# language ViewPatterns, EmptyCase, TypeApplications, RankNTypes, TypeFamilyDependencies, TypeFamilies, GADTs, ScopedTypeVariables, UndecidableInstances, ImpredicativeTypes, OverloadedRecordDot, QuasiQuotes, NoMonomorphismRestriction, DataKinds, FlexibleContexts , TemplateHaskell #-}

{-# language RankNTypes, FlexibleInstances, MultiParamTypeClasses, FunctionalDependencies, ExistentialQuantification, ExplicitForAll, ImpredicativeTypes, RankNTypes #-}

{-# Language RebindableSyntax, OverloadedRecordDot, GADTs, TemplateHaskell, ImpredicativeTypes, FlexibleContexts, UndecidableInstances, FlexibleInstances #-}

module Impl.TH where

import Data.Traversable
import Data.Data.Lens
import Control.Lens
import Language.Haskell.TH.Lens hiding (name)
import Data.Map (Map)
import qualified Data.Map as Map
import Prelude hiding (exp)
import Data.Time
import GHC.Records
import Language.Haskell.TH

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
    selfName <- newName "self"
    realDecs <- map (fixDecs typ selfName) <$> decsQ
    let stuff = collectDecs realDecs
    for (Map.toList stuff) $ \(name, decs) -> do
        let
            methodName =
                litT $ strTyLit $ nameBase $ name
            methodTypes =
                toListOf
                    (traverse . _SigD . _2)
                    decs
            implementations = do
                dec <- decs
                case dec of
                    FunD _ clauses ->
                        pure $ FunD 'getField clauses
                    ValD _ expr decs' ->
                        pure $ FunD 'getField [Clause [VarP selfName] expr decs']
                    _ ->
                        []

        methodType <-
            case methodTypes of
                [mt] ->
                    pure mt
                _ ->
                    fail $ "Expected a type for " <> show name <> ", got: " <> show methodTypes

        [stub] <-
            [d|
                instance HasField $(methodName) $(conT typ) $(pure methodType) where

                |]
        pure $ case stub of
            InstanceD moverlap preds typ' _ ->
                InstanceD moverlap preds typ' implementations
            _ ->
                error "internal error"


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
    replaceSelf = transform $ \x -> case x of
        UnboundVarE nm | nm == mkName "self" ->
            VarE selfName
        _ ->
            x

    addSelfLambda :: Body -> Body
    addSelfLambda = transformOn biplate replaceSelf

    addSelfClause :: Clause -> Clause
    addSelfClause (transformOn biplate replaceSelf -> Clause patterns body decs) =
        Clause ((VarP selfName) : patterns) body decs

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
                SigD name typ
            KiSigD name typ ->
                KiSigD name (addSelfType typ)
            InfixD fixity name ->
                InfixD fixity name

            -- it would be cool to support some of these
            ForeignD _ ->
                error "foreign unsupported"
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
