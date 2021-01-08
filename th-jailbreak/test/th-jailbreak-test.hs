{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

import Data.ByteString.Builder
import Language.Haskell.TH.Jailbreak
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main =
  defaultMain $
    testCase
      "cassava"
      ( toLazyByteString
          ( $(importHidden "cassava" "Data.Csv.Conversion.Internal" "formatDecimal")
              @Int
              114514
          )
          @?= "114514"
      )
