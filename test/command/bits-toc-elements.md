```
% pandoc -f jats -t native
<toc-group>
	<toc-title-group>
		<title>TOC group</title>
	</toc-title-group>
	<p>Content of toc group</p>
	<toc>
		<toc-title-group>
			<title>TOC</title>
		</toc-title-group>
		<p>Content of TOC</p>
  		<toc-div content-type="sections">
   			<toc-title-group>
                <title>Mental Health Services</title>
            </toc-title-group>
            <toc-entry>
                <label>Section 1</label>
                <title>Introduction</title>
                <nav-pointer rid="tc3"/>
            </toc-entry>
            <toc-entry>
                <label>Section 2</label>
                <title>Mental Health of the Population</title>
                <nav-pointer rid="tc4"/>
            </toc-entry>
  		</toc-div>
	</toc>
</toc-group>
^D
[ Header
    1 ( "" , [] , [] ) [ Str "TOC" , Space , Str "group" ]
, Para
    [ Str "Content"
    , Space
    , Str "of"
    , Space
    , Str "toc"
    , Space
    , Str "group"
    ]
, Header 2 ( "" , [] , [] ) [ Str "TOC" ]
, Para
    [ Str "Content" , Space , Str "of" , Space , Str "TOC" ]
, Header
    3
    ( "" , [] , [] )
    [ Str "Mental"
    , Space
    , Str "Health"
    , Space
    , Str "Services"
    ]
, Header 4 ( "" , [] , [] ) [ Str "Introduction" ]
, Header
    4
    ( "" , [] , [] )
    [ Str "Mental"
    , Space
    , Str "Health"
    , Space
    , Str "of"
    , Space
    , Str "the"
    , Space
    , Str "Population"
    ]
]
```