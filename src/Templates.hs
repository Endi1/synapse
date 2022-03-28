{-# LANGUAGE OverloadedStrings #-}

module Templates (noteTemplate) where
import Text.Blaze.Html5 as H
import Types (Note (_nIdentifier, _nCompiledContent))
import Text.Blaze.Html5.Attributes (href, rel, class_)

noteTemplate :: Note -> Html
noteTemplate n = docTypeHtml $ do
    H.head $ do
        H.title (toHtml $ _nIdentifier n)
        link ! href "style/pico.min.css" ! rel "stylesheet"
    body $ do
        H.main ! class_ "container" $ do
            article $ do
                header $ toHtml $ _nIdentifier n
                preEscapedText $ _nCompiledContent n