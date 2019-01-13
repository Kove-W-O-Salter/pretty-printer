{-|
Module      : Text.PrettyPrint.Internal
Description : Backend for 'pretty-printer'.
Copyright   : (c) Kove W. Ochre-Salter
License     : MIT
Maintainer  : kove.w.o.salter@gmail.com
Stability   : experimental
Portability : TemplateHaskell

= 'Text.PrettyPrinter.Internal'.

This module exports all of the internal data-structures, classes
and functions for 'pretty-printer'.
-}


{-# LANGUAGE TemplateHaskell #-}
module Text.PrettyPrinter.Internal
  (
    -- * Types and Classes.
    PrettyPrinter
  , Indent
  , Data
  , Modify
  , IsPrettyPrinter (..)

    -- * Runnners.
  , prettyPrint

    -- * State wrappers.
  , get
  , modify
  , getIndent
  , modifyIndent
  , getText
  , modifyText
  ) where


import           Control.Lens                    ((^.), (&), (%~), _1, _2)
import           Control.Monad
import           Control.Monad.State             (State, runState)
import qualified Control.Monad.State as S        (get, modify)
import           Data.Monoid                     (mempty)
import           Data.String                     (IsString, fromString)
import           Prelude                  hiding (print)


-- | == Types and Clasess.


-- | A state 'Monad' for storing a pretty printer's state; see 'Data'.
newtype PrettyPrinter s a = PrettyPrinter (State (Data s) a)


-- | Indentation; the number of space to insert when 'indent' is called.
type Indent = Int


-- | 'PrettyPrinter''s internal data; indentation paired with a stream of text.
type Data s = (Indent, s)


-- | A modification, or repmapping, of some type.
type Modify a = a -> a


-- | All important 'PrettyPrinter' functions for formatting and text-insertion.
class (IsString s, Monoid s) => IsPrettyPrinter s where
  -- | Insert the indentation into the text-stream followed by 'p'.
  indent :: PrettyPrinter s a -> PrettyPrinter s a
  indent p = do
    t <- getIndent
    text $ fromString $ replicate t ' '
    p

  -- | Run 'p' and then insert a space into the text-stream.
  space :: PrettyPrinter s a -> PrettyPrinter s a
  space p = do
    r <- p
    text $ fromString " "
    return r

  -- | Run 'p' and then insert a line into the text-stream.
  line :: PrettyPrinter s a -> PrettyPrinter s a
  line p = do
    r <- p
    text $ fromString "\n"
    return r

  -- | Insert 's' into the text-stream.
  text :: s -> PrettyPrinter s ()
  text s = modifyText (<> s)

  -- | Locally modify the Indentation.
  with :: Indent -> PrettyPrinter s a -> PrettyPrinter s a
  with t p = do
    modifyIndent (+ t)
    r <- p
    modifyIndent (`subtract` t)
    return r

  -- | Run 'p' preceded by insertion of 'o' and followed by the insertion of 's'.
  wrap :: s -> s -> PrettyPrinter s a -> PrettyPrinter s a
  wrap o c p = do
    text o
    r <- p
    text c
    return r


-- Monad instance.
instance IsPrettyPrinter s => Monad (PrettyPrinter s) where
  pX >>= pF = PrettyPrinter $
    state pX >>= state . pF


-- Applicative instance,
instance IsPrettyPrinter s => Applicative (PrettyPrinter s) where
  pure = PrettyPrinter . pure

  pF <*> pX = do
    f <- pF
    x <- pX
    return $ f x


-- Functor instance.
instance IsPrettyPrinter s => Functor (PrettyPrinter s) where
  fmap f = (pure f <*>)


-- | == Runners.


-- | Run a 'PrettyPrinter', discarding the resulting indentation and return the text-stream.
prettyPrint :: IsPrettyPrinter s => PrettyPrinter s a -> s
prettyPrint p = snd $ snd $ runState (state p) (0, mempty)


-- | == State Wrappers


-- | Remove a 'PrettyPrinter's constructor, returning the underlying 'State'.
state :: IsPrettyPrinter s => PrettyPrinter s a -> State (Data s) a
state (PrettyPrinter s) = s


-- | Wrapper for 'S.get'.
get :: IsPrettyPrinter s => PrettyPrinter s (Data s)
get = PrettyPrinter S.get


-- | Wrapper for 'S.modify'.
modify :: IsPrettyPrinter s => Modify (Data s) -> PrettyPrinter s ()
modify = PrettyPrinter . S.modify


-- | Get the indentation from a 'PrettyPrinter''s 'Data'.
getIndent :: IsPrettyPrinter s => PrettyPrinter s Indent
getIndent = (^. _1) <$> get


-- | Modify the indentation of a 'PrettyPrinter''s 'Data'.
modifyIndent :: IsPrettyPrinter s => Modify Indent -> PrettyPrinter s ()
modifyIndent m = modify (& _1 %~ m)


-- | Get the text-stream from a 'PrettyPrinter''s 'Data'.
getText :: IsPrettyPrinter s => PrettyPrinter s s
getText = (^. _2) <$> get


-- | Modify the text-stream of a 'PrettyPrinter''s 'Data'.
modifyText :: IsPrettyPrinter s => Modify s -> PrettyPrinter s ()
modifyText m = modify (& _2 %~ m)
