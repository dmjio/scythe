{-# LANGUAGE RecordWildCards #-}
module Data.CSV.LexerUtils where

import           Control.Monad.State
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LT
import           Data.ByteString          (ByteString)
import qualified Data.ByteString          as B
import           Data.ByteString.Builder
import qualified Data.ByteString.Internal as B (w2c)
import qualified Data.ByteString.Lazy     as L
import           Data.Text                (Text)
import qualified Data.Text                as T
import qualified Data.Text.Encoding       as T
import           Data.Word
import           GHC.Int

appendMode :: Action
appendMode len bs = do
  s@LexerState {..} <- get
  put s { stringBuffer
            = stringBuffer
            <> (lazyByteString
             $ L.take len (alexStr bs))
        }
  pure Nothing

endMode :: Action
endMode _ _ = do
  mode <- gets lexerMode
  case mode of
    InNormal -> pure Nothing
    InString -> apply
  where
    apply = do
      buf <- toLazyByteString <$> gets stringBuffer
      modify $ \s -> s
        { lexerMode = InNormal
        , stringBuffer = mempty
        }
      pure $ Just (Item buf)

data LexerMode
  = InNormal
  | InString
  deriving (Show, Eq)

data Token
  = Item !L.ByteString
  | Newline
  | Comma
  | TokenError !Error
  deriving (Show, Eq)

type Action = Int64 -> AlexInput -> State LexerState (Maybe Token)

data AlexInput = AlexInput
  { alexChar    :: {-# UNPACK #-} !Char
  , alexStr     :: !L.ByteString
  , alexBytePos :: {-# UNPACK #-} !Int
  } deriving (Show, Eq)

data LexerState
  = LexerState
  { matchedInput :: {-# UNPACK #-} !AlexInput
  , lexerMode    :: !LexerMode
  , stringBuffer :: !Builder
  }

item :: Action
item inputLength _ = do
  LexerState {..} <- get
  pure $ Just $ Item $
    L.take inputLength (alexStr matchedInput)

token t = \_ _ ->
  pure (Just t)

errorAction :: AlexInput -> State LexerState [Token]
errorAction AlexInput {..} =
  pure [TokenError (LexerError (LT.unpack $ LT.decodeUtf8 alexStr))]

data Error
  = LexerError String
  | UntermString
  deriving (Show, Eq)

eofAction :: State LexerState [Token]
eofAction = do
  mode <- gets lexerMode
  pure $ case mode of
    InString      -> [TokenError UntermString]
    InNormal      -> []

alexGetByte :: AlexInput -> Maybe (Word8, AlexInput)
alexGetByte AlexInput {..} =
  case L.uncons alexStr of
    Nothing -> Nothing
    Just (c, rest) ->
      Just (c, AlexInput {
        alexChar = B.w2c c,
        alexStr = rest,
        alexBytePos = alexBytePos+1
      })

startString :: Action
startString _ _ =
  Nothing <$ do
    modify $ \s -> s { lexerMode = InString }

alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar = alexChar
