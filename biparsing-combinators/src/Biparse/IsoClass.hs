module Biparse.IsoClass (IsoClass(..)) where

class IsoClass a p where iso :: Iso p a
