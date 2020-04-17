{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{- |
Module      : Text.Pandoc.Lua.Walk
Copyright   : © 2012–2020 John MacFarlane,
              © 2017-2020 Albert Krewinkel
License     : GNU GPL, version 2 or above
Maintainer  : Albert Krewinkel <tarleb+pandoc@moltkeplatz.de>
Stability   : alpha

Walking documents in a filter-suitable way.
-}
module Text.Pandoc.Lua.Walk
  ( SingletonsList (..)
  )
where

import Control.Monad ((<=<))
import Text.Pandoc.Definition
import Text.Pandoc.Walk

-- | Helper type which allows to traverse trees in order, while splicing in
-- trees.
--
-- The only interesting use of this type is via it's '@Walkable@' instance. That
-- instance makes it possible to walk a Pandoc document (or a subset thereof),
-- while applying a function on each element of an AST element /list/, and have
-- the resulting list spliced back in place of the original element. This is the
-- traversal/splicing method used for Lua filters.
newtype SingletonsList a = SingletonsList { singletonsList :: [a] }
  deriving (Functor, Foldable, Traversable)

--
-- SingletonsList Inline
--
instance {-# OVERLAPPING #-} Walkable (SingletonsList Inline) [Inline] where
  walkM = walkSingletonsListM
  query = querySingletonsList

instance Walkable (SingletonsList Inline) Pandoc where
  walkM = walkPandocM
  query = queryPandoc

instance Walkable (SingletonsList Inline) Citation where
  walkM = walkCitationM
  query = queryCitation

instance Walkable (SingletonsList Inline) Inline where
  walkM = walkInlineM
  query = queryInline

instance Walkable (SingletonsList Inline) Block where
  walkM = walkBlockM
  query = queryBlock

instance Walkable (SingletonsList Inline) Row where
  walkM = walkRowM
  query = queryRow

instance Walkable (SingletonsList Inline) TableHead where
  walkM = walkTableHeadM
  query = queryTableHead

instance Walkable (SingletonsList Inline) TableBody where
  walkM = walkTableBodyM
  query = queryTableBody

instance Walkable (SingletonsList Inline) TableFoot where
  walkM = walkTableFootM
  query = queryTableFoot

instance Walkable (SingletonsList Inline) Caption where
  walkM = walkCaptionM
  query = queryCaption

instance Walkable (SingletonsList Inline) Cell where
  walkM = walkCellM
  query = queryCell

instance Walkable (SingletonsList Inline) MetaValue where
  walkM = walkMetaValueM
  query = queryMetaValue

instance Walkable (SingletonsList Inline) Meta where
  walkM f (Meta metamap) = Meta <$> walkM f metamap
  query f (Meta metamap) = query f metamap

--
-- SingletonsList Block
--
instance {-# OVERLAPPING #-} Walkable (SingletonsList Block) [Block] where
  walkM = walkSingletonsListM
  query = querySingletonsList

instance Walkable (SingletonsList Block) Pandoc where
  walkM = walkPandocM
  query = queryPandoc

instance Walkable (SingletonsList Block) Citation where
  walkM = walkCitationM
  query = queryCitation

instance Walkable (SingletonsList Block) Inline where
  walkM = walkInlineM
  query = queryInline

instance Walkable (SingletonsList Block) Block where
  walkM = walkBlockM
  query = queryBlock

instance Walkable (SingletonsList Block) Row where
  walkM = walkRowM
  query = queryRow

instance Walkable (SingletonsList Block) TableHead where
  walkM = walkTableHeadM
  query = queryTableHead

instance Walkable (SingletonsList Block) TableBody where
  walkM = walkTableBodyM
  query = queryTableBody

instance Walkable (SingletonsList Block) TableFoot where
  walkM = walkTableFootM
  query = queryTableFoot

instance Walkable (SingletonsList Block) Caption where
  walkM = walkCaptionM
  query = queryCaption

instance Walkable (SingletonsList Block) Cell where
  walkM = walkCellM
  query = queryCell

instance Walkable (SingletonsList Block) MetaValue where
  walkM = walkMetaValueM
  query = queryMetaValue

instance Walkable (SingletonsList Block) Meta where
  walkM f (Meta metamap) = Meta <$> walkM f metamap
  query f (Meta metamap) = query f metamap


walkSingletonsListM :: (Monad m, Walkable (SingletonsList a) a)
                    => (SingletonsList a -> m (SingletonsList a))
                    -> [a] -> m [a]
walkSingletonsListM f =
  let f' = fmap singletonsList . f . SingletonsList . (:[]) <=< walkM f
  in fmap mconcat . mapM f'

querySingletonsList :: (Monoid c, Walkable (SingletonsList a) a)
                    => (SingletonsList a -> c)
                    -> [a] -> c
querySingletonsList f =
  let f' x = f (SingletonsList [x]) `mappend` query f x
  in mconcat . map f'
