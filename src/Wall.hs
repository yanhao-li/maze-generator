module Wall where
import Control.DeepSeq

data Wall = Wall {
  r :: Int,
  c :: Int
} deriving (Show, Eq, Ord)

instance NFData Wall where
  rnf (Wall r c) = rnf r `seq` rnf c