{-# LANGUAGE OverloadedStrings #-}

module Templates (index, synapse) where

import Control.Monad (forM_)
import RIO (Int64)
import RIO.Text.Lazy (Text)
import Text.Blaze.Html ((!))
import Text.Blaze.Html5 qualified as H
import Text.Blaze.Html5.Attributes (href)

index :: [(Int64, Text)] -> H.Html
index synapseHeaders = H.docTypeHtml $ do
  H.head $ do
    H.title "Hello Synapse!"
  H.body $ do
    H.h1 "Hello world"
    H.ul $ forM_ synapseHeaders (\(i, t) -> H.li $ (H.a ! href (H.toValue $ "/" ++ show i)) $ H.toHtml t)

synapse :: (Int64, Text) -> H.Html
synapse (_, rowText) = H.docTypeHtml $ do
  H.head $ do
    H.title "Hello Synapse!"
  H.body $ do
    H.h1 "Hello world"
    H.p $ H.toHtml rowText