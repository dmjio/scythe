{
{-# LANGUAGE RecordWildCards #-}
module Data.CSV.Lexer (getCSV, Token (..))where

import           Data.ByteString.Lazy
import           Data.CSV.LexerUtils
import           Control.Monad.State

}

$TEXT = [\x0020-\x0021\x0023-\x002B\x002D-\x007E]
@escaped = ($TEXT | \, | \r | \n | \"\")*

scythe :-
  <0> {
    $TEXT+ { item }
    \, ;
    \n { token Newline }
    \r\n { token Newline }
    \" { startString }
  }
  <string> {
    @escaped { appendMode }
    \" { endMode }
  }

{

mkInput :: ByteString -> AlexInput
mkInput s = AlexInput '\n' s 0

alexScanTokens :: ByteString -> [Token]
alexScanTokens input = flip evalState (LexerState (mkInput input) InNormal mempty) go
  where
    go :: State LexerState [Token]
    go = do
      LexerState {..} <- get
      case alexScan matchedInput (stateToInt lexerMode) of
        AlexEOF -> eofAction
        AlexError alexInput -> errorAction alexInput
        AlexSkip alexInput _ -> do
          modify $ \s -> s { matchedInput = alexInput }
          go
        AlexToken alexInput _ act -> do
          let len = fromIntegral $ alexBytePos alexInput - alexBytePos matchedInput
          r <- act len matchedInput
          modify $ \s -> s { matchedInput = alexInput }
          case r of
            Nothing -> go
            Just t -> do
              ts <- go
              pure (t:ts)

getCSV :: ByteString -> [Token]
getCSV = alexScanTokens

stateToInt :: LexerMode -> Int
stateToInt InNormal{} = 0
stateToInt InString{} = string

}
