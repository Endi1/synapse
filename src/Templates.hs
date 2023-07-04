{-# LANGUAGE OverloadedStrings #-}

module Templates (index, synapse) where

import Control.Monad (forM_)
import RIO (Int64)
import RIO.Text.Lazy (Text)
import Text.Blaze.Html5 qualified as H

index :: [Text] -> H.Html
index synapseHeaders = H.docTypeHtml $ do
  H.head $ do
    H.title "Hello Synapse!"
  H.body $ do
    H.h1 "Hello world"
    H.ul $ forM_ synapseHeaders (H.li . H.toHtml)

synapse :: (Int64, Text) -> H.Html
synapse (_, rowText) = H.docTypeHtml $ do
  H.head $ do
    H.title "Hello Synapse!"
  H.body $ do
    H.h1 "Hello world"
    H.p $ H.toHtml rowText