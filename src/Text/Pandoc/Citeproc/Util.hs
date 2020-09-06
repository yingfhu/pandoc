{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Text.Pandoc.Citeproc.Util
 ( caseTransform
 , toIETF )
where
import Citeproc.CaseTransform
import Text.Pandoc.Definition
import Text.Pandoc.Builder as B
import Text.Pandoc.Walk as Walk
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Sequence as Seq
import Data.Char (isSpace, isAlphaNum, isAscii)
import Control.Monad.State

caseTransform :: (CaseTransformState -> Text -> Text)
              -> Inlines
              -> Inlines
caseTransform f x =
  evalState (caseTransform' f x) Start


-- custom traversal which does not descend into
-- SmallCaps, Superscript, Subscript, Span "nocase" (implicit nocase)
caseTransform' :: (CaseTransformState -> Text -> Text)
               -> Inlines
               -> State CaseTransformState Inlines
caseTransform' f ils =
  case Seq.viewr (unMany ils) of
    xs Seq.:> Str t | not (Seq.null xs)
                    , not (hasWordBreak t) -> do
        xs' <- mapM go xs
        st <- get
        when (st == AfterWordEnd || st == StartSentence || st == Start) $
          put BeforeLastWord
        x' <- go (Str t)
        return $ Many $ xs' Seq.|> x'
    _ -> mapM go ils
 where
  go (Str t) = Str . mconcat <$> mapM g (splitUp t)
  go Space = Space <$ g " "
  go (SmallCaps zs) = return' $ SmallCaps zs
  go (Superscript zs) = return' $ Superscript zs
  go (Subscript zs) = return' $ Subscript zs
  go (Span attr@(_,classes,_) zs)
      | "nocase" `elem` classes = do
            st <- get
            case st of
              AfterWordChar | classes == ["nocase"]
                   -- we need to apply g to update the state:
                -> return' $ Span nullAttr zs
              _ -> return' $ Span attr zs
      | otherwise = Span attr <$> mapM go zs
  go (Emph zs) = Emph <$> mapM go zs
  go (Underline zs) = Underline <$> mapM go zs
  go (Strong zs) = Strong <$> mapM go zs
  go (Strikeout zs) = Strikeout <$> mapM go zs
  go (Quoted qt zs) = Quoted qt <$> mapM go zs
  go (Cite cs zs) = Cite cs <$> mapM go zs
  go (Link attr zs t) = (\x -> Link attr x t) <$> mapM go zs
  go (Image attr zs t) = (\x -> Image attr x t) <$> mapM go zs
  go x = return x

  -- we need to apply g to update the state:
  return' x = x <$ g (Walk.query fromStr x)

  fromStr (Str t) = t
  fromStr _ = mempty

  g :: Text -> State CaseTransformState Text
  g t = do
    st <- get
    put $ case T.unsnoc t of
            Nothing -> st
            Just (_,c)
              | c == '.' || c == '?' || c == '!' || c == ':' ->
                AfterSentenceEndingPunctuation
              | isAlphaNum c -> AfterWordChar
              | isSpace c
              , st == AfterSentenceEndingPunctuation -> StartSentence
              | isWordBreak c -> AfterWordEnd
              | otherwise -> AfterOtherPunctuation
    return $
      if T.all isAlphaNum t
         then f st t
         else t
  isWordBreak '-' = True
  isWordBreak '/' = True
  isWordBreak '\x2013' = True
  isWordBreak '\x2014' = True
  isWordBreak c = isSpace c
  hasWordBreak = T.any isWordBreak
  splitUp = T.groupBy sameType
  sameType c d =
    -- note that non-English characters get treated differently
    -- by titlecase transformation
    (isAscii c && isAlphaNum c && isAscii d && isAlphaNum d) ||
    (not (isAscii c) && isAlphaNum c && not (isAscii d) && isAlphaNum d) ||
    (isSpace c && isSpace d)

toIETF :: Text -> Text
toIETF "english"         = "en-US" -- "en-EN" unavailable in CSL
toIETF "usenglish"       = "en-US"
toIETF "american"        = "en-US"
toIETF "british"         = "en-GB"
toIETF "ukenglish"       = "en-GB"
toIETF "canadian"        = "en-US" -- "en-CA" unavailable in CSL
toIETF "australian"      = "en-GB" -- "en-AU" unavailable in CSL
toIETF "newzealand"      = "en-GB" -- "en-NZ" unavailable in CSL
toIETF "afrikaans"       = "af-ZA"
toIETF "arabic"          = "ar"
toIETF "basque"          = "eu"
toIETF "bulgarian"       = "bg-BG"
toIETF "catalan"         = "ca-AD"
toIETF "croatian"        = "hr-HR"
toIETF "czech"           = "cs-CZ"
toIETF "danish"          = "da-DK"
toIETF "dutch"           = "nl-NL"
toIETF "estonian"        = "et-EE"
toIETF "finnish"         = "fi-FI"
toIETF "canadien"        = "fr-CA"
toIETF "acadian"         = "fr-CA"
toIETF "french"          = "fr-FR"
toIETF "francais"        = "fr-FR"
toIETF "austrian"        = "de-AT"
toIETF "naustrian"       = "de-AT"
toIETF "german"          = "de-DE"
toIETF "germanb"         = "de-DE"
toIETF "ngerman"         = "de-DE"
toIETF "greek"           = "el-GR"
toIETF "polutonikogreek" = "el-GR"
toIETF "hebrew"          = "he-IL"
toIETF "hungarian"       = "hu-HU"
toIETF "icelandic"       = "is-IS"
toIETF "italian"         = "it-IT"
toIETF "japanese"        = "ja-JP"
toIETF "latvian"         = "lv-LV"
toIETF "lithuanian"      = "lt-LT"
toIETF "magyar"          = "hu-HU"
toIETF "mongolian"       = "mn-MN"
toIETF "norsk"           = "nb-NO"
toIETF "nynorsk"         = "nn-NO"
toIETF "farsi"           = "fa-IR"
toIETF "polish"          = "pl-PL"
toIETF "brazil"          = "pt-BR"
toIETF "brazilian"       = "pt-BR"
toIETF "portugues"       = "pt-PT"
toIETF "portuguese"      = "pt-PT"
toIETF "romanian"        = "ro-RO"
toIETF "russian"         = "ru-RU"
toIETF "serbian"         = "sr-RS"
toIETF "serbianc"        = "sr-RS"
toIETF "slovak"          = "sk-SK"
toIETF "slovene"         = "sl-SL"
toIETF "spanish"         = "es-ES"
toIETF "swedish"         = "sv-SE"
toIETF "thai"            = "th-TH"
toIETF "turkish"         = "tr-TR"
toIETF "ukrainian"       = "uk-UA"
toIETF "vietnamese"      = "vi-VN"
toIETF "latin"           = "la"
toIETF x                 = x

