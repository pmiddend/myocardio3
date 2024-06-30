module Util (packShow, packShowLazy) where

import Data.Function ((.))
import Data.Text (Text, pack)
import Data.Text.Lazy qualified as TL
import Text.Show (Show (show))

packShow :: (Show a) => a -> Text
packShow = pack . show

packShowLazy :: (Show a) => a -> TL.Text
packShowLazy = TL.pack . show
