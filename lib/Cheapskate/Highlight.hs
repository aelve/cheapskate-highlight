{-# LANGUAGE
RecordWildCards
  #-}


module Cheapskate.Highlight
(
  -- * Highlighting
  highlightDoc,
  highlightDocWith,
  highlightBlock,
  highlightBlockWith,

  -- * Options
  FormatOptions(..),
  defaultFormatOpts,

  -- * Styles
  module Text.Highlighting.Kate.Styles,
  styleToCss,
)
where


import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.Text.Lazy as TL
import Cheapskate
import Text.Highlighting.Kate
import Text.Highlighting.Kate.Styles
import Text.Blaze.Html.Renderer.Text


{- |
Highlight a document by replacing code blocks with raw HTML blocks.

By default, the rest of the attribute line (i.e. all words after the first word after @~~~@ or @```@) get added as classes to the container block of the code.
-}
highlightDoc :: Doc -> Doc
highlightDoc = highlightDocWith (\_ _ x -> x)

{- |
The function is given code block's language (i.e. the 1st word of the attribute line after @~~~@ or @```@) and the rest of the attribute line.

If you don't want the classes to be derived from the attribute line, make the function set 'containerClasses' to @[]@.
-}
highlightDocWith
  :: (Text -> Text -> FormatOptions -> FormatOptions) -> Doc -> Doc
highlightDocWith f doc = walk (highlightBlockWith f) doc

highlightBlock :: Block -> Block
highlightBlock = highlightBlockWith (\_ _ x -> x)

highlightBlockWith
  :: (Text -> Text -> FormatOptions -> FormatOptions) -> Block -> Block
highlightBlockWith f (CodeBlock CodeAttr{..} code) =
  HtmlBlock (TL.toStrict (renderHtml formatted))
  where
    lang = T.unpack codeLang
    highlighted = highlightAs lang (T.unpack code)
    opts = defaultFormatOpts {
             containerClasses = words (T.unpack codeInfo) }
    formatted = formatHtmlBlock (f codeLang codeInfo opts) highlighted
highlightBlockWith _ other = other
