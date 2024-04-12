module Main where

import Cardano.Binary qualified as CBOR
import Data.Aeson (KeyValue ((.=)), object)
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Bifunctor (
  first,
 )
import Data.ByteString.Base16 qualified as Base16
import Data.ByteString.Lazy qualified as LBS
import Data.Text (
  Text,
  pack,
 )
import Data.Text.Encoding qualified as Text
import Plutarch (
  Config (Config),
  TracingMode (DoTracing),
  compile,
 )
import Plutarch.Api.V2 (PPubKeyHash)
import Plutarch.Evaluate (
  evalScript,
 )
import "liqwid-plutarch-extra" Plutarch.Extra.Script (
  applyArguments,
 )
import Plutarch.Prelude
import Plutarch.Script (Script, serialiseScript)
import PlutarchCampaign (campaignvalidator)
import PlutusLedgerApi.V2 (
  Data,
  ExBudget,
  PubKeyHash,
 )

-- import Sample

encodeSerialiseCBOR :: Script -> Text
encodeSerialiseCBOR = Text.decodeUtf8 . Base16.encode . CBOR.serialize' . serialiseScript

evalT :: ClosedTerm a -> Either Text (Script, ExBudget, [Text])
evalT x = evalWithArgsT x []

evalWithArgsT :: ClosedTerm a -> [Data] -> Either Text (Script, ExBudget, [Text])
evalWithArgsT x args = do
  cmp <- compile (Config DoTracing) x
  let (escr, budg, trc) = evalScript $ applyArguments cmp args
  scr <- first (pack . show) escr
  pure (scr, budg, trc)

writePlutusScript :: String -> FilePath -> ClosedTerm a -> IO ()
writePlutusScript title filepath term = do
  case evalT term of
    Left e -> putStrLn (show e)
    Right (script, _, _) -> do
      let scriptType = "PlutusScriptV2" :: String
          plutusJson = object ["type" .= scriptType, "description" .= title, "cborHex" .= encodeSerialiseCBOR script]
          content = encodePretty plutusJson
      LBS.writeFile filepath content

-- pdatarec :: Term s (PDataRecord ["campaignmanager" ':=  PPubKeyHash, "campaignnumber" ':= PInteger])
-- pdatarec = pdcons # pdata "4c307016ae804a61430bb4b05b7196b4f7549fc34b31d7b6ef16850f" #$ pdcons # pdata 1 # pdnil

-- campaign :: Term s (PCampaign)
-- campaign = pconstant (PCampaign pdatarec)

pkh :: Term s (PPubKeyHash)
pkh = pconstant ("34071a108c7804754836e006e3331196ddbe7e4b8808fe077ec9e537" :: PubKeyHash)

main :: IO ()
main = do
  -- writePlutusScript "gift" "./compiled/gift.plutus" gift
  -- writePlutusScript "burn" "./compiled/burn.plutus" burn
  -- writePlutusScript "campaignvalidator" "./compiled/campaignvalidator.plutus" campaignvalidator
  writePlutusScript "campaignvalidator" "./compiled/campaignvalidator1.plutus" (campaignvalidator # pkh)

-- writePlutusScript "campaignvalidator2" "./compiled/campaignvalidator2.plutus" (campaignvalidator2 # campaign)
