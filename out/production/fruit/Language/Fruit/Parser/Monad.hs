{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE Rank2Types #-}

module Language.Fruit.Parser.Monad
  ( -- * Parsing monad
    P,
    execParser,
    PState (..),

    -- * Monadic operations
    askState,
    getPState,
    setPState,
    getStartCode,
    setStartCode,
    getPosition,
    setPosition,
    getCommentDepth,
    setCommentDepth,
    getInput,
    setInput,
    popToken,
    pushToken,

    -- * Error reporting
    ParseFail (..),
    parseError,
  )
where

import Control.Monad.Fail (MonadFail (fail))
import Data.String (unwords)
import GHC.Show (showParen, showString, showsPrec)
import Language.Fruit.Data.InputStream (InputStream)
import Language.Fruit.Data.Position (Position)
import qualified Language.Fruit.Data.Position as Pos
import Language.Fruit.Data.Span (Spanned)
import Language.Fruit.Syntax.Tok (Tok)
import Prelude hiding (fail, unwords)

-- | Parsing and lexing monad. A value of type @'P' a@ represents a parser that can be run (using
-- 'execParser') to possibly produce a value of type @a@.
newtype P a
  = P
      { unParser ::
          forall r.
          PState -> -- State being passed along
          (a -> PState -> r) -> -- Successful parse continuation
          (String -> Position -> r) -> -- Failed parse continuation
          r -- Final output
      }

-- | State that the lexer and parser share
data PState
  = PState
      { curPos :: !Position,
        curInput :: !InputStream,
        startCode :: !Int,
        prevPos :: Maybe Position,
        pushedIndents :: [Natural],
        pushedTokens :: [Spanned Tok],
        commentDepth :: !Natural
      }
  deriving (Show)

instance Functor P where
  fmap f m = P $ \ !s pOk pFailed -> unParser m s (pOk . f) pFailed

instance Applicative P where
  pure x = P $ \ !s pOk _ -> pOk x s

  m <*> k = P $ \ !s pOk pFailed ->
    let pOk' x s' = unParser k s' (pOk . x) pFailed
     in unParser m s pOk' pFailed

instance Monad P where
  return = pure

  m >>= k = P $ \ !s pOk pFailed ->
    let pOk' x s' = unParser (k x) s' pOk pFailed
     in unParser m s pOk' pFailed

instance MonadFail P where
  fail msg = P $ \ !s _ pFailed -> pFailed msg (curPos s)

-- | Exceptions that occur during parsing
data ParseFail = ParseFail Position String
  deriving (Eq, Typeable)

instance Show ParseFail where
  showsPrec p (ParseFail pos msg) =
    showParen (p >= 11) (showString err)
    where
      err =
        unwords
          [ "parse failure at",
            Pos.prettyPrint pos,
            "(" ++ msg ++ ")"
          ]

instance Exception ParseFail

execParser :: P a -> InputStream -> Position -> Either ParseFail a
execParser parser input pos =
  unParser
    parser
    initialState
    (\result _ -> Right result)
    (\message errPos -> Left (ParseFail errPos message))
  where
    initialState =
      PState
        { curPos = pos,
          curInput = input,
          startCode = 0,
          prevPos = Nothing,
          pushedIndents = [1],
          pushedTokens = [],
          commentDepth = 0
        }

askState :: (PState -> a) -> P a
askState f = f <$> getPState

-- | Extract the state stored in the parser.
getPState :: P PState
getPState = P $ \ !s pOk _ -> pOk s s

-- | Update the state stored in the parser.
setPState :: PState -> P ()
setPState s = P $ \_ pOk _ -> pOk () s

-- | Get Alex's start code'
getStartCode :: P Int
getStartCode = askState startCode

-- | Set Alex's start code
setStartCode :: Int -> P ()
setStartCode sc = modifyPState (\s -> s {startCode = sc})

-- | Modify the state stored in the parser.
modifyPState :: (PState -> PState) -> P ()
modifyPState f = P $ \ !s pOk _ -> pOk () (f $! s)

-- | Retrieve the current position of the parser.
getPosition :: P Position
getPosition = askState curPos

-- | Update the current position of the parser.
setPosition :: Position -> P ()
setPosition pos = modifyPState $ \s -> s {curPos = pos}

-- | Retrieve the current 'InputStream' of the parser.
getInput :: P InputStream
getInput = askState curInput

-- | Update the current 'InputStream' of the parser.
setInput :: InputStream -> P ()
setInput i = modifyPState $ \s -> s {curInput = i}

getCommentDepth :: P Natural
getCommentDepth = askState commentDepth

setCommentDepth :: Natural -> P ()
setCommentDepth depth = modifyPState (\s -> s {commentDepth = depth})

-- | Manually push a @'Spanned' 'Tok'@.
pushToken :: Spanned Tok -> P ()
pushToken tok =
  modifyPState $ \s@PState {pushedTokens} ->
    s {pushedTokens = tok : pushedTokens}

-- | Manually pop a @'Spanned' 'Tok'@
-- (if there are no tokens to pop, returns 'Nothing').
-- See 'pushToken' for more details.
popToken :: P (Maybe (Spanned Tok))
popToken = P $ \parserState@PState {pushedTokens} pOk _ ->
  pOk
    (listToMaybe pushedTokens)
    parserState {pushedTokens = drop 1 pushedTokens}

-- | Signal a syntax error.
parseError :: Show b => b -> P a
parseError b =
  fail $
    "Syntax error: unexpected token `" <> show b <> "`"
