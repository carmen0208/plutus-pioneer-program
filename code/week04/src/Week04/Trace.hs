{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds        #-}

module Week04.Trace where

import Control.Monad.Freer.Extras as Extras
import Data.Default               (Default (..))
import Data.Functor               (void)
import Ledger
import Ledger.TimeSlot
import Plutus.Trace.Emulator      as Emulator
import Wallet.Emulator.Wallet

import Week04.Vesting

-- Contract w s e a
-- EmulatorTrace a

test :: IO ()
test = runEmulatorTraceIO myTrace

myTrace :: EmulatorTrace ()
myTrace = do
    -- this is the wallet 1
    h1 <- activateContractWallet (Wallet 1) endpoints
    -- this is the wallet 2
    h2 <- activateContractWallet (Wallet 2) endpoints
    --
    callEndpoint @"give" h1 $ GiveParams
        { gpBeneficiary = pubKeyHash $ walletPubKey $ Wallet 2
        , gpDeadline    = slotToBeginPOSIXTime def 20
        , gpAmount      = 10000000
        }
    void $ waitUntilSlot 20
    callEndpoint @"grab" h2 ()
    s <- waitNSlots 1
    Extras.logInfo $ "reached " ++ show s

-- Notes:
--  :l src/Week04/Trace.hs
-- import Plutus.Contract.Trace
--  :i InitialDistribution
-- defaultDist
-- defaultDistFor [Wallet 1, Wallet 2]
--  import Plutus.Trace.Emulator
-- import Data.Default
-- :i EmulatorConfig
--  def :: EmulatorConfig

------------------------------------------------
-- import Ledger.Fee
-- import Data.Default
--  :i FeeConfig
--  def :: FeeConfig
-- import Plutus.Trace.Emulator
-- :i runEmulatorTrace
-- runEmulatorTrace def def $ return ()
-- :t runEmulatorTraceIO
-- runEmulatorTraceIO $ return ()