```
% pandoc -f jats -t native
<fig id="fig_A.1" orientation="portrait">
 <label>Figure A.1</label>
 <caption>
  <title>Field of Application</title>
 </caption>
 <legend>
  <title>Key</title>
  <def-list>
   <def-item>
    <term>I</term>
    <def><p>input</p></def>
   </def-item>
   <def-item>
    <term>O</term>
    <def><p>output</p></def>
   </def-item>
   ...
  </def-list>
 </legend>
 <graphic xlink:href="1234"/>
</fig>
^D
[ Figure
    ( "fig_A.1" , [] , [] )
    (Caption
       Nothing
       [ Plain
           [ Str "Field"
           , Space
           , Str "of"
           , Space
           , Str "Application"
           ]
       ])
    [ Header 1 ( "" , [] , [] ) [ Str "Key" ]
    , DefinitionList
        [ ( [ Str "I" ] , [ [ Para [ Str "input" ] ] ] )
        , ( [ Str "O" ] , [ [ Para [ Str "output" ] ] ] )
        ]
    , Para [ Image ( "" , [] , [] ) [] ( "1234" , "" ) ]
    ]
]
```