{-# LANGUAGE OverloadedStrings #-}
module Text.Pandoc.Citeproc.CslJson
  ( readCslJson
  , writeCslJson )
where

import Citeproc.CslJson
import Citeproc.Types
import Control.Monad.Identity (runIdentity)
import           Data.Aeson.Encode.Pretty         (Config (..), Indent (Spaces),
                                                   NumberFormat (Generic),
                                                   defConfig, encodePretty')
import Data.Aeson (eitherDecodeStrict')
import Data.ByteString.Lazy (toStrict)
import Data.ByteString (ByteString)
import Text.Pandoc.Builder as B
import Data.Text (Text)
import qualified Data.Text as T

fromCslJson :: CslJson Text -> Inlines
fromCslJson (CslText t) = B.text t
fromCslJson CslEmpty = mempty
fromCslJson (CslConcat x y) = fromCslJson x <> fromCslJson y
fromCslJson (CslQuoted x) = B.doubleQuoted (fromCslJson x)
fromCslJson (CslItalic x) = B.emph (fromCslJson x)
fromCslJson (CslBold x) = B.strong (fromCslJson x)
fromCslJson (CslUnderline x) = B.underline (fromCslJson x)
fromCslJson (CslNoDecoration x) =
  B.spanWith ("",["nodecoration"],[]) (fromCslJson x)
fromCslJson (CslSmallCaps x) = B.smallcaps (fromCslJson x)
fromCslJson (CslBaseline x) = fromCslJson x
fromCslJson (CslSub x) = B.subscript (fromCslJson x)
fromCslJson (CslSup x) = B.superscript (fromCslJson x)
fromCslJson (CslNoCase x) = B.spanWith ("",["nocase"],[]) (fromCslJson x)
fromCslJson (CslDiv t x) = B.spanWith ("",["csl-" <> t],[]) (fromCslJson x)

toCslJson :: Inlines -> CslJson Text
toCslJson = foldMap fromInline

fromInlines :: [Inline] -> CslJson Text
fromInlines = toCslJson . B.fromList

fromInline :: Inline -> CslJson Text
fromInline (Str t) = CslText t
fromInline (Emph ils) = CslItalic (fromInlines ils)
fromInline (Strong ils) = CslBold (fromInlines ils)
fromInline (Underline ils) = CslUnderline (fromInlines ils)
fromInline (Strikeout ils) = fromInlines ils
fromInline (Superscript ils) = CslSup (fromInlines ils)
fromInline (Subscript ils) = CslSub (fromInlines ils)
fromInline (SmallCaps ils) = CslSmallCaps (fromInlines ils)
fromInline (Quoted _ ils) = CslQuoted (fromInlines ils)
fromInline (Cite _ ils) = fromInlines ils
fromInline (Code _ t) = CslText t
fromInline Space = CslText " "
fromInline SoftBreak = CslText " "
fromInline LineBreak = CslText "\n"
fromInline (Math _ t) = CslText t
fromInline (RawInline _ _) = CslEmpty
fromInline (Link _ ils _) = fromInlines ils
fromInline (Image _ ils _) = fromInlines ils
fromInline (Note _) = CslEmpty
fromInline (Span (_,[cl],_) ils)
  | "csl-" `T.isPrefixOf` cl = CslDiv cl (fromInlines ils)
fromInline (Span _ ils) = fromInlines ils

readCslJson :: ByteString -> Either String [Reference Inlines]
readCslJson raw =
  case eitherDecodeStrict' raw of
    Left e        -> Left e
    Right cslrefs -> Right $
      map (runIdentity . traverse (return . fromCslJson)) cslrefs

writeCslJson :: Locale -> [Reference Inlines] -> ByteString
writeCslJson locale = toStrict .
  encodePretty' defConfig{ confIndent = Spaces 2
                         , confCompare = compare
                         , confNumFormat = Generic }
  . map (runIdentity .  traverse (return . renderCslJson locale . toCslJson))
