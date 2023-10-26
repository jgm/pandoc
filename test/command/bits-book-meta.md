```
% pandoc -f jats -t native -s
<book>
    <book-meta>
		<book-id book-id-type="publisher">handbook-648</book-id>
  		<book-title-group>
   			<book-title>The NCBI Handbook</book-title>
  		</book-title-group>
  		<contrib-group>
   			<contrib contrib-type="author">
    			<name><surname>McEntyre</surname>
     			<given-names>Jo</given-names></name>
    			<xref ref-type="aff" rid="bid.m.1"/>
   			</contrib>
   			<contrib contrib-type="editor">
    			<name><surname>Ostell</surname>
     			<given-names>Jim</given-names></name>
    			<xref ref-type="aff" rid="bid.m.1"/>
   			</contrib>
  		</contrib-group>
  		<aff id="bid.m.1">
   			<institution>National Center for Biotechnology Information (NCBI), National Library of Medicine, National Institutes of Health</institution>, <addr-line>Bethesda, MD 20892-6510</addr-line>
  		</aff>
  		<pub-date iso-8601-date="2002-11">
   			<month>11</month>
   			<year>2002</year>
  		</pub-date>
  		<publisher>
   			<publisher-name>National Center for Biotechnology Information (NCBI), National Library of Medicine, National Institutes of Health</publisher-name>
   			<publisher-loc>Bethesda, MD</publisher-loc>
  		</publisher>
  		<edition>1</edition>
  		<counts>
   			<book-fig-count count="98"/>
   			<book-table-count count="40"/>
   			<book-equation-count count="0"/>
   			<book-ref-count count="115"/>
   			<book-page-count count="532"/>
   			<book-word-count count="149852"/>
  		</counts>
 	</book-meta>
</book>
^D
Pandoc
  Meta
    { unMeta =
        fromList
          [ ( "author"
            , MetaList
                [ MetaInlines [ Str "Jo" , Space , Str "McEntyre" ] ]
            )
          , ( "date" , MetaInlines [ Str "2002-11" ] )
          , ( "institute"
            , MetaList
                [ MetaInlines
                    [ Str "National"
                    , Space
                    , Str "Center"
                    , Space
                    , Str "for"
                    , Space
                    , Str "Biotechnology"
                    , Space
                    , Str "Information"
                    , Space
                    , Str "(NCBI),"
                    , Space
                    , Str "National"
                    , Space
                    , Str "Library"
                    , Space
                    , Str "of"
                    , Space
                    , Str "Medicine,"
                    , Space
                    , Str "National"
                    , Space
                    , Str "Institutes"
                    , Space
                    , Str "of"
                    , Space
                    , Str "Health,"
                    , Space
                    , Str "Bethesda,"
                    , Space
                    , Str "MD"
                    , Space
                    , Str "20892-6510"
                    ]
                ]
            )
          , ( "title"
            , MetaInlines
                [ Str "The"
                , Space
                , Str "NCBI"
                , Space
                , Str "Handbook"
                ]
            )
          ]
    }
  []
```