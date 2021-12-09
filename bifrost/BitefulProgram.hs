module BitefulProgram where

import qualified Data.Map as M
import Types
import PrettyPrint

findBiteful :: BitefulProgram -> BiteLabel -> Maybe Biteful
findBiteful prog label = M.lookup label $ bitp_bites prog

mustFindBiteful :: BitefulProgram -> BiteLabel -> Biteful
mustFindBiteful prog label =
  case findBiteful prog label of
    Just bite -> bite
    Nothing -> error $ "Could not find bite: " ++ show label
