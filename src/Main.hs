{-# LANGUAGE OverloadedStrings #-}
import Data.Monoid (mappend)
import Hakyll
import Text.Pandoc.Options

main :: IO ()
main = hakyll $ do
    match "templates/*" $
        compile templateCompiler
    
    match (fromRegex "pages/.*\\.md") $ do
        route $ gsubRoute "pages/" (const "") `composeRoutes` setExtension "html"
        compile $ pandocCompilerWith defaultHakyllReaderOptions (defaultHakyllWriterOptions {writerHTMLMathMethod = MathJax ""})
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    match "static/css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match "static/download/*" $ do
        route   idRoute
        compile copyFileCompiler
    
    match "keybase.txt" $ do
        route   idRoute
        compile copyFileCompiler
