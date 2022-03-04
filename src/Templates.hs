{-# LANGUAGE OverloadedStrings #-}

module Templates (noteTemplate) where
import Text.Blaze.Html5 as H
import Types (Note (nIdentifier, nCompiledContent))
import Text.Blaze.Html5.Attributes (href, rel)

noteTemplate :: Note -> Html
noteTemplate n = docTypeHtml $ do
    H.head $ do
        H.title (toHtml $ nIdentifier n)
        link ! href "style/style.css" ! rel "stylesheet"
    body $ preEscapedText $ nCompiledContent n