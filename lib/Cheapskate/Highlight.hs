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
  styleToLaTeX,
)
where


import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Cheapskate
import Text.Highlighting.Kate
import Text.Highlighting.Kate.Styles
import Text.Blaze.Html.Renderer.Text


highlightDoc :: Doc -> Doc
highlightDoc doc = walk highlightBlock doc

highlightDocWith :: FormatOptions -> Doc -> Doc
highlightDocWith opts doc = walk (highlightBlockWith opts) doc

highlightBlock :: Block -> Block
highlightBlock (CodeBlock attr code) =
  HtmlBlock (TL.toStrict (renderHtml formatted))
  where
    lang = T.unpack (codeLang attr)
    highlighted = highlightAs lang (T.unpack code)
    formatted = formatHtmlBlock defaultFormatOpts highlighted
highlightBlock other = other

highlightBlockWith :: FormatOptions -> Block -> Block
highlightBlockWith opts (CodeBlock attr code) =
  HtmlBlock (TL.toStrict (renderHtml formatted))
  where
    lang = T.unpack (codeLang attr)
    highlighted = highlightAs lang (T.unpack code)
    formatted = formatHtmlBlock opts highlighted
highlightBlockWith _ other = other
