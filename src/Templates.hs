{-# LANGUAGE OverloadedStrings #-}

module Templates (noteTemplate) where
import Text.Blaze.Html5 as H
import Types (Note (nIdentifier, nCompiledContent))
import Text.Blaze.Html5.Attributes (href, rel, class_)

noteTemplate :: Note -> Html
noteTemplate n = docTypeHtml $ do
    H.head $ do
        H.title (toHtml $ nIdentifier n)
        link ! href "style/pico.min.css" ! rel "stylesheet"
    body $ do
        H.main ! class_ "container" $ do
            preEscapedText $ nCompiledContent n