return {
  {
    Meta = function(meta)
      -- The call to `MetaInlines` is redundant and used for testing purposes
      -- only. The explicit use of a MetaValue constructor is only useful when
      -- used with an empty table: `MetaInlines{}` is read differently than
      -- `MetaBlocks{}`.
      meta.title = pandoc.MetaInlines{pandoc.SmallCaps(meta.title)}
      return meta
    end
  }
}
