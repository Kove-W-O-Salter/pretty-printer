{-|
Module      : Text.PrettyPrinter
Description : Frontend for 'pretty-printer'
Copyright   : (c) Kove W. Ochre-Salter
License     : MIT
Maintainer  : kove.w.o.salter@gmail.com
Stability   : experimental
Portability : TypeSynonymInstances, FlexibleInstances

= 'Text.PrettyPrinter'.

This module simply re-exports a selection of the 'Text.PrettyPrinter.Internal'
and instances many 'IsString s => s' of 'IsPrettyPrinter'.
-}


{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Text.PrettyPrinter
  (
    -- * Types and Classes.
    PrettyPrinter
  , Indent
  , IsPrettyPrinter (..)

    -- * Functions.
  , prettyPrint
  ) where


import qualified Data.Text                   as ST (Text)
import qualified Data.Text.Lazy              as LT (Text)
import qualified Data.ByteString             as SB (ByteString)
import qualified Data.ByteString.Lazy        as LB (ByteString)
import           Text.PrettyPrinter.Internal


instance IsPrettyPrinter String        -- String instance.
instance IsPrettyPrinter ST.Text       -- Strict Text instance.
instance IsPrettyPrinter LT.Text       -- Lazy Text instance.
instance IsPrettyPrinter SB.ByteString -- Strict ByteString instance.
instance IsPrettyPrinter LB.ByteString -- Lazy ByteString instance.
