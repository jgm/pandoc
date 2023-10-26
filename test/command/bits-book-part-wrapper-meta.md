```
% pandoc -f jats -t native -s
<book-part-wrapper 
  dtd-version="2.1"
  xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
  xmlns:xi="http://www.w3.org/2001/XInclude"
  xmlns:mml="http://www.w3.org/1998/Math/MathML"
  xmlns:xlink="http://www.w3.org/1999/xlink">

 <collection-meta>
  <title-group>
   <title>Balisage Series on Markup Technologies</title>
  </title-group>
 
  <abstract>
   <p>The <italic>Balisage Series on Markup Technologies</italic> 
    is an occasional series...</p>
  </abstract>
 </collection-meta>

 <book-meta>
  <book-title-group>
   <book-title>Proceedings of Balisage: The Markup Conference 
    2013</book-title>
  </book-title-group>
 
  <abstract>
   <p>Balisage is a peer-reviewed conference...</p></abstract>
 
 </book-meta>
</book-part-wrapper>
^D
Pandoc
  Meta
    { unMeta =
        fromList
          [ ( "abstract"
            , MetaBlocks
                [ Para
                    [ Str "Balisage"
                    , Space
                    , Str "is"
                    , Space
                    , Str "a"
                    , Space
                    , Str "peer-reviewed"
                    , Space
                    , Str "conference..."
                    ]
                ]
            )
          , ( "title"
            , MetaInlines
                [ Str "Proceedings"
                , Space
                , Str "of"
                , Space
                , Str "Balisage:"
                , Space
                , Str "The"
                , Space
                , Str "Markup"
                , Space
                , Str "Conference"
                , SoftBreak
                , Str "2013"
                ]
            )
          ]
    }
  [ Para
      [ Str "The"
      , Space
      , Emph
          [ Str "Balisage"
          , Space
          , Str "Series"
          , Space
          , Str "on"
          , Space
          , Str "Markup"
          , Space
          , Str "Technologies"
          ]
      , SoftBreak
      , Str "is"
      , Space
      , Str "an"
      , Space
      , Str "occasional"
      , Space
      , Str "series..."
      ]
  ]
```