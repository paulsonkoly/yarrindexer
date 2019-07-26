module YarrIndexer.Types
  ( ClassType(..)
  , Class(..)
  , Method(..)
  )
where


import           Data.Text                      ( Text )


data ClassType = C | M deriving Show
data Class = Class { ctype :: ClassType , link :: Text , name :: Text }
  deriving Show

data Method = Method { link' :: Text, name' :: Text }
  deriving Show
