--------------------------------------------------------------------
-- |
-- Module    : Text.XML.Light.Cursor
-- Copyright : (c) Galois, Inc. 2008
-- License   : BSD3
--
-- Maintainer: Iavor S. Diatchki <diatchki@galois.com>
-- Stability : provisional
-- Portability:
--
-- XML cursors for working XML content withing the context of
-- an XML document.  This implemntation is based on the general
-- tree zipper written by Krasimir Angelov and Iavor S. Diatchki.
--

module Text.XML.Light.Cursor
  ( Tag(..), getTag, setTag, fromTag
  , Cursor(..), Path

  -- * Conversions
  , fromContent
  , fromElement
  , fromForest
  , toForest
  , toTree

  -- * Moving around
  , parent
  , root
  , getChild
  , firstChild
  , lastChild
  , left
  , right

  -- ** Searching
  , findChild
  , findLeft
  , findRight

  -- * Node classification
  , isRoot
  , isFirst
  , isLast
  , isLeaf
  , isChild
  , hasChildren
  , getNodeIndex

  -- * Updates
  , setContent
  , modifyContent
  , modifyContentM

  -- ** Inserting content
  , insertLeft
  , insertRight
  , insertGoLeft
  , insertGoRight

  -- ** Removing content
  , removeLeft
  , removeRight
  , removeGoLeft
  , removeGoRight
  , removeGoUp

  ) where

import Text.XML.Light.Types
import Data.Maybe(isNothing)

data Tag = Tag { tagName    :: QName
               , tagAttribs :: [Attr]
               , tagLine    :: Maybe Line
               } deriving (Show)

getTag :: Element -> Tag
getTag e = Tag { tagName = elName e
               , tagAttribs = elAttribs e
               , tagLine = elLine e
               }

setTag :: Tag -> Element -> Element
setTag t e = fromTag t (elContent e)

fromTag :: Tag -> [Content] -> Element
fromTag t cs = Element { elName    = tagName t
                       , elAttribs = tagAttribs t
                       , elLine    = tagLine t
                       , elContent = cs
                       }

type Path = [([Content],Tag,[Content])]

-- | The position of a piece of content in an XML document.
data Cursor = Cur
  { current :: Content      -- ^ The currently selected content.
  , lefts   :: [Content]    -- ^ Siblings on the left, closest first.
  , rights  :: [Content]    -- ^ Siblings on the right, closest first.
  , parents :: Path -- ^ The contexts of the parent elements of this location.
  } deriving (Show)

-- Moving around ---------------------------------------------------------------

-- | The parent of the given location.
parent :: Cursor -> Maybe Cursor
parent loc =
  case parents loc of
    (pls,v,prs) : ps -> Just
      Cur { current = Elem
                    (fromTag v
                    (combChildren (lefts loc) (current loc) (rights loc)))
          , lefts = pls, rights = prs, parents = ps
          }
    [] -> Nothing


-- | The top-most parent of the given location.
root :: Cursor -> Cursor
root loc = maybe loc root (parent loc)

-- | The left sibling of the given location.
left :: Cursor -> Maybe Cursor
left loc =
  case lefts loc of
    t : ts -> Just loc { current = t, lefts = ts
                                    , rights = current loc : rights loc }
    []     -> Nothing

-- | The right sibling of the given location.
right :: Cursor -> Maybe Cursor
right loc =
  case rights loc of
    t : ts -> Just loc { current = t, lefts = current loc : lefts loc
                                    , rights = ts }
    []     -> Nothing

-- | The first child of the given location.
firstChild :: Cursor -> Maybe Cursor
firstChild loc =
  do (t : ts, ps) <- downParents loc
     return Cur { current = t, lefts = [], rights = ts , parents = ps }

-- | The last child of the given location.
lastChild :: Cursor -> Maybe Cursor
lastChild loc =
  do (ts, ps) <- downParents loc
     case reverse ts of
       l : ls -> return Cur { current = l, lefts = ls, rights = []
                                                     , parents = ps }
       [] -> Nothing

-- | Find the next left sibling that satisfies a predicate.
findLeft :: (Cursor -> Bool) -> Cursor -> Maybe Cursor
findLeft p loc = do loc1 <- left loc
                    if p loc1 then return loc1 else findLeft p loc1

-- | Find the next right sibling that satisfies a predicate.
findRight :: (Cursor -> Bool) -> Cursor -> Maybe Cursor
findRight p loc = do loc1 <- right loc
                     if p loc1 then return loc1 else findRight p loc1

-- | The first child that satisfies a predicate.
findChild :: (Cursor -> Bool) -> Cursor -> Maybe Cursor
findChild p loc =
  do loc1 <- firstChild loc
     if p loc1 then return loc1 else findRight p loc1

-- | The child with the given index (starting from 0).
getChild :: Int -> Cursor -> Maybe Cursor
getChild n loc =
  do (ts,ps) <- downParents loc
     (ls,t,rs) <- splitChildren ts n
     return Cur { current = t, lefts = ls, rights = rs, parents = ps }


-- | private: computes the parent for "down" operations.
downParents :: Cursor -> Maybe ([Content], Path)
downParents loc =
  case current loc of
    Elem e -> Just ( elContent e
                   , (lefts loc, getTag e, rights loc) : parents loc
                   )
    _      -> Nothing

-- Conversions -----------------------------------------------------------------

-- | A cursor for the guven content.
fromContent :: Content -> Cursor
fromContent t = Cur { current = t, lefts = [], rights = [], parents = [] }

-- | A cursor for the guven element.
fromElement :: Element -> Cursor
fromElement e = fromContent (Elem e)

-- | The location of the first tree in a forest.
fromForest :: [Content] -> Maybe Cursor
fromForest (t:ts) = Just Cur { current = t, lefts = [], rights = ts
                                                      , parents = [] }
fromForest []     = Nothing

-- | Computes the tree containing this location.
toTree :: Cursor -> Content
toTree loc = current (root loc)

-- | Computes the forest containing this location.
toForest :: Cursor -> [Content]
toForest loc = let r = root loc in combChildren (lefts r) (current r) (rights r)


-- Queries ---------------------------------------------------------------------

-- | Are we at the top of the document?
isRoot :: Cursor -> Bool
isRoot loc = null (parents loc)

-- | Are we at the left end of the the document?
isFirst :: Cursor -> Bool
isFirst loc = null (lefts loc)

-- | Are we at the right end of the document?
isLast :: Cursor -> Bool
isLast loc = null (rights loc)

-- | Are we at the bottom of the document?
isLeaf :: Cursor -> Bool
isLeaf loc = isNothing (downParents loc)

-- | Do we have a parent?
isChild :: Cursor -> Bool
isChild loc = not (isRoot loc)

-- | Get the node index inside the sequence of children
getNodeIndex :: Cursor -> Int
getNodeIndex loc = length (lefts loc)

-- | Do we have children?
hasChildren :: Cursor -> Bool
hasChildren loc = not (isLeaf loc)



-- Updates ---------------------------------------------------------------------

-- | Change the current content.
setContent :: Content -> Cursor -> Cursor
setContent t loc = loc { current = t }

-- | Modify the current content.
modifyContent :: (Content -> Content) -> Cursor -> Cursor
modifyContent f loc = setContent (f (current loc)) loc

-- | Modify the current content, allowing for an effect.
modifyContentM :: Monad m => (Content -> m Content) -> Cursor -> m Cursor
modifyContentM f loc = do x <- f (current loc)
                          return (setContent x loc)

-- | Insert content to the left of the current position.
insertLeft :: Content -> Cursor -> Cursor
insertLeft t loc = loc { lefts = t : lefts loc }

-- | Insert content to the right of the current position.
insertRight :: Content -> Cursor -> Cursor
insertRight t loc = loc { rights = t : rights loc }

-- | Remove the conent on the left of the current position, if any.
removeLeft :: Cursor -> Maybe (Content,Cursor)
removeLeft loc = case lefts loc of
                   l : ls -> return (l,loc { lefts = ls })
                   [] -> Nothing

-- | Remove the conent on the right of the current position, if any.
removeRight :: Cursor -> Maybe (Content,Cursor)
removeRight loc = case rights loc of
                    l : ls -> return (l,loc { rights = ls })
                    [] -> Nothing


-- | Insert content to the left of the current position.
-- The new content becomes the current position.
insertGoLeft :: Content -> Cursor -> Cursor
insertGoLeft t loc = loc { current = t, rights = current loc : rights loc }

-- | Insert content to the right of the current position.
-- The new content becomes the current position.
insertGoRight :: Content -> Cursor -> Cursor
insertGoRight t loc = loc { current = t, lefts = current loc : lefts loc }

-- | Remove the current element.
-- The new position is the one on the left.
removeGoLeft :: Cursor -> Maybe Cursor
removeGoLeft loc = case lefts loc of
                     l : ls -> Just loc { current = l, lefts = ls }
                     []     -> Nothing

-- | Remove the current element.
-- The new position is the one on the right.
removeGoRight :: Cursor -> Maybe Cursor
removeGoRight loc = case rights loc of
                     l : ls -> Just loc { current = l, rights = ls }
                     []     -> Nothing

-- | Remove the current element.
-- The new position is the parent of the old position.
removeGoUp :: Cursor -> Maybe Cursor
removeGoUp loc =
  case parents loc of
    (pls,v,prs) : ps -> Just
      Cur { current = Elem (fromTag v (reverse (lefts loc) ++ rights loc))
          , lefts = pls, rights = prs, parents = ps
          }
    [] -> Nothing


-- | private: Gets the given element of a list.
-- Also returns the preceeding elements (reversed) and the folloing elements.
splitChildren :: [a] -> Int -> Maybe ([a],a,[a])
splitChildren _ n | n < 0 = Nothing
splitChildren cs pos = loop [] cs pos
  where loop acc (x:xs) 0 = Just (acc,x,xs)
        loop acc (x:xs) n = loop (x:acc) xs $! n-1
        loop _ _ _        = Nothing

-- | private: combChildren ls x ys = reverse ls ++ [x] ++ ys
combChildren :: [a] -> a -> [a] -> [a]
combChildren ls t rs = foldl (flip (:)) (t:rs) ls
