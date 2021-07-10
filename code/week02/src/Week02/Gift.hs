{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

module Week02.Gift where

import           Control.Monad       hiding (fmap)
import           Data.Map            as Map
import           Data.Text           (Text)
import           Data.Void           (Void)
import           Plutus.Contract
import           PlutusTx            (Data (..))
import qualified PlutusTx
import           PlutusTx.Prelude    hiding (Semigroup(..), unless)
import           Ledger              hiding (singleton)
import           Ledger.Constraints  as Constraints
import qualified Ledger.Scripts      as Scripts
import           Ledger.Ada          as Ada
import           Playground.Contract (printJson, printSchemas, ensureKnownCurrencies, stage)
import           Playground.TH       (mkKnownCurrencies, mkSchemaDefinitions)
import           Playground.Types    (KnownCurrency (..))
import           Prelude             (IO, Semigroup (..), String)
import           Text.Printf         (printf)

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

{-# INLINABLE mkValidator #-}
-- * () is like void, means there's no output that we care for this function
-- * this is most simple validator(alway pass), doesn't validate anything
-- That's why call this Gift, as it not validation, anybody can send anything to anybody else
mkValidator :: Data -> Data -> Data -> ()
mkValidator _ _ _ = ()

-- TODO: understand Template Haskell 
-- * mkValidatorScript that is from Ledger.Scripts
--     you can check the type by run `cabal repl` and `:t mkValidatorScript`
-- * [\\?????\\] is the quote syntax, it take expression and converted in to a syntax tree
-- $$ splice is takes the syntax tree of the template and splice it into the code
validator :: Validator
validator = mkValidatorScript $$(PlutusTx.compile [|| mkValidator ||])

valHash :: Ledger.ValidatorHash
valHash = Scripts.validatorHash validator

-- This is a address on the blockchain(script addresses)
scrAddress :: Ledger.Address
scrAddress = scriptAddress validator

-- Endpoint for users to trigger something
--     give take int param, grab doesn't take any params
-- give is for people to put money in
-- grab is for spend from this address
type GiftSchema =
            Endpoint "give" Integer
        .\/ Endpoint "grab" ()

give :: AsContractError e => Integer -> Contract w s e ()
give amount = do
    -- this must pay amount of lovelance to valHash address, with Datum
    let tx = mustPayToOtherScript valHash (Datum $ Constr 0 []) $ Ada.lovelaceValueOf amount
    -- this submit this transaction
    ledgerTx <- submitTx tx
    -- this line wait for confirmation of this transaction
    void $ awaitTxConfirmed $ txId ledgerTx
    -- this line logs this information
    logInfo @String $ printf "made a gift of %d lovelace" amount

grab :: forall w s e. AsContractError e => Contract w s e ()
grab = do
    -- Looks up all UTXO's setting this address.
    utxos <- utxoAt scrAddress
    -- get all their references
    let orefs   = fst <$> Map.toList utxos
    -- looks up to tell how to find this 
        lookups = Constraints.unspentOutputs utxos      <>
                  Constraints.otherScript validator
        -- define the transactions, must spend that UTXO
        tx :: TxConstraints Void Void
        tx      = mconcat [mustSpendScriptOutput oref $ Redeemer $ I 17 | oref <- orefs]
    -- compiler allow the wallet to lookup this transactions by find UTXOs and the actual validation script
    ledgerTx <- submitTxConstraintsWith @Void lookups tx
    void $ awaitTxConfirmed $ txId ledgerTx
    logInfo @String $ "collected gifts"

endpoints :: Contract () GiftSchema Text ()
endpoints = (give' `select` grab') >> endpoints
  where
    give' = endpoint @"give" >>= give
    grab' = endpoint @"grab" >>  grab

mkSchemaDefinitions ''GiftSchema

mkKnownCurrencies []
