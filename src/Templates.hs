{-# LANGUAGE OverloadedStrings #-}

module Templates (index, synapse) where

import CMarkGFM (commonmarkToHtml)
import Control.Monad (forM_)
import Text.Blaze.Html ((!))
import Text.Blaze.Html5 qualified as H
import Text.Blaze.Html5.Attributes (href)
import Types qualified (Synapse (..))

index :: [Types.Synapse] -> H.Html
index synapses = H.docTypeHtml $ do
  H.head $ do
    H.title "Hello Synapse!"
  H.body $ do
    H.h1 "Hello world"
    H.ul $ forM_ synapses (\s -> H.li $ (H.a ! href (H.toValue $ "/" ++ show (Types.id s))) $ H.toHtml (Types.name s))

synapse :: Types.Synapse -> H.Html
synapse synapseRow = H.docTypeHtml $ do
  H.head $ do
    H.title $ H.toHtml $ Types.name synapseRow
  H.body $ do
    H.preEscapedToHtml $ commonmarkToHtml [] [] $ Types.content synapseRow