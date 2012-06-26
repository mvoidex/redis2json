module Database.Redis.Dump.JSON (
	object,
	getObject,
	getObjects,
	dump
	) where

import Control.Arrow
import Control.Monad
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as L
import qualified Data.Aeson as A
import qualified Data.Aeson.Encode.Pretty as A
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.HashMap.Strict as HM
import Data.Monoid
import Data.Either
import qualified Database.Redis as R

-- | Convert pairs of key-value to object
object :: [(ByteString, ByteString)] -> A.Object
object = HM.fromList . map (toText *** toValue) where
	toText = T.decodeUtf8
	toValue = A.String . toText

-- | Get object from Redis by key as key-value pair
getObject :: (R.RedisCtx m f, Functor m, Functor f) => T.Text -> m (f A.Object)
getObject = fmap (fmap object) . R.hgetall . T.encodeUtf8

getObject' :: (R.RedisCtx m f, Functor m, Functor f) => T.Text -> m (f A.Object)
getObject' k = fmap (fmap (HM.singleton k . A.Object)) $ getObject k
-- | Get objects by key pattern
getObjects :: T.Text -> R.Redis [A.Object]
getObjects p = do
	ks <- R.keys (T.encodeUtf8 p)
	case ks of
		Left _ -> return []
		Right ks' -> fmap rights $ mapM (getObject' . T.decodeUtf8) ks'

-- | Dump objects by key pattern
dump :: T.Text -> R.Redis L.ByteString
dump = fmap (A.encodePretty . mconcat) . getObjects
