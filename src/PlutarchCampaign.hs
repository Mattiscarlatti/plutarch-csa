module PlutarchCampaign (PCampaign (..), campaignvalidator, campaignvalidator2) where

import Plutarch (Term, perror, plam, pmatch, (#))
import Plutarch.Api.V1 (PRedeemer)
import Plutarch.Api.V2 (PDatum, PPubKeyHash, PScriptContext, PScriptPurpose (PSpending))
import Plutarch.DataRepr (PDataFields)
import Plutarch.Monadic qualified as P
import Plutarch.Prelude (DPTStrat, DerivePlutusType, PAsData, PData, PDataRecord, PInteger, PIsData, PLabeledType ((:=)), PTryFrom, PUnit, PlutusType, PlutusTypeData, S, pconstant, pdata, pelem, pfield, pfromData, pif, pletFields, type (:-->))
import PlutusPrelude (Generic)

data PCampaign (s :: S)
  = PCampaign
      ( Term
          s
          ( PDataRecord
              '[ "campaignmanager" ':= PPubKeyHash
               , "campaignnumber" ':= PInteger
               ]
          )
      )
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PDataFields)

instance DerivePlutusType PCampaign where
  type DPTStrat _ = PlutusTypeData

instance PTryFrom PData PCampaign

campaignvalidator :: Term s (PPubKeyHash :--> PAsData PDatum :--> PAsData PRedeemer :--> PAsData PScriptContext :--> PUnit)
campaignvalidator = plam $ \pkh _ _ ctx' -> P.do
  ctx <- pletFields @["txInfo", "purpose"] ctx'
  PSpending _ <- pmatch ctx.purpose
  let signatories = pfield @"signatories" # ctx.txInfo
  pif (pelem # pdata pkh # pfromData signatories) (pconstant ()) perror

campaignvalidator2 :: Term s (PCampaign :--> PAsData PDatum :--> PAsData PRedeemer :--> PAsData PScriptContext :--> PUnit)
campaignvalidator2 = plam $ \cmpgn' _ _ ctx' -> P.do
  ctx <- pletFields @["txInfo", "purpose"] ctx'
  cmpgn <- pletFields @["campaignmanager", "campaignnumber"] cmpgn'
  PSpending _ <- pmatch ctx.purpose
  let signatories = pfield @"signatories" # ctx.txInfo
  pif (pelem # pdata cmpgn.campaignmanager # pfromData signatories) (pconstant ()) perror
