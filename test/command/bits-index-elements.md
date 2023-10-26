```
% pandoc -f jats -t native
<index-group>
	<index-title-group>
		<title>Index group</title>
	</index-title-group>
	<p>Content of index group</p>
	<index>
		<index-title-group>
			<title>Index</title>
		</index-title-group>
		<p>Content of index</p>
  		<index-div>
   			<index-title-group>
    			<title>N</title>
   			</index-title-group>
   			<p>Content of index div</p>
   			<index-entry>
    			<term>Navy</term>
    			<x>.</x>
    			<see-entry>Armed forces</see-entry>
    			<x>.</x>
   			</index-entry>
   			<index-entry>
    			<term>Necessary and proper clause, congressional power</term>
   			</index-entry>
   			<index-entry>
    			<term>Newsgathering as commerce</term>
   			</index-entry>
  		</index-div>
	</index>
</index-group>
^D
[ Header
    1 ( "" , [] , [] ) [ Str "Index" , Space , Str "group" ]
, Para
    [ Str "Content"
    , Space
    , Str "of"
    , Space
    , Str "index"
    , Space
    , Str "group"
    ]
, Header 2 ( "" , [] , [] ) [ Str "Index" ]
, Para
    [ Str "Content" , Space , Str "of" , Space , Str "index" ]
, Header 3 ( "" , [] , [] ) [ Str "N" ]
, Para
    [ Str "Content"
    , Space
    , Str "of"
    , Space
    , Str "index"
    , Space
    , Str "div"
    ]
, Plain [ Str "Navy" ]
, Plain [ Str "." ]
, Plain [ Str "Armed" , Space , Str "forces" ]
, Plain [ Str "." ]
, Plain
    [ Str "Necessary"
    , Space
    , Str "and"
    , Space
    , Str "proper"
    , Space
    , Str "clause,"
    , Space
    , Str "congressional"
    , Space
    , Str "power"
    ]
, Plain
    [ Str "Newsgathering"
    , Space
    , Str "as"
    , Space
    , Str "commerce"
    ]
]
```