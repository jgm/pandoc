```
% pandoc -f jats -t native
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

 <book-part id="bid.1" book-part-type="part">
   			<book-part-meta>
    			<title-group>
     				<label>Part 1</label>
     				<title>The Databases</title>
    			</title-group>
   			</book-part-meta>
   			<body>
    			<sec id="bid.3">
     				<title>History</title>
     				<p>Initially, GenBank was built and maintained at Los Alamos 
      				National Laboratory.</p>
    			</sec>
   			</body>
   			<back>
                <title>Back matter of book part</title>
  				<ref-list>
   					<title>References</title>
   					<ref id="bid.41">
    					<label>1</label>
    					<element-citation>
     						<person-group>
      							<string-name>
									<surname>Olson</surname>
       								<given-names>M</given-names>
								</string-name>
      							<string-name>
									<surname>Hood</surname>
       								<given-names>L</given-names>
								</string-name>
      							<string-name>
									<surname>Cantor</surname>
       								<given-names>C</given-names>
								</string-name>
      							<string-name>
									<surname>Botstein</surname>
       								<given-names>D</given-names>
								</string-name>
     						</person-group>
     						<article-title>A common language for physical mapping of the human genome</article-title>
     						<source>Science</source>
     						<year iso-8601-date="1989">1989</year>
     						<volume>245</volume>
     						<issue>4925</issue>
     						<fpage>1434</fpage>
     						<lpage>1435</lpage>
     						<pub-id pub-id-type="pmid">2781285</pub-id>
    					</element-citation>
   					</ref>
  				</ref-list>
 			</back>
  	</book-part>
</book-part-wrapper>
^D
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
, Header 2 ( "bid.3" , [] , [] ) [ Str "History" ]
, Para
    [ Str "Initially,"
    , Space
    , Str "GenBank"
    , Space
    , Str "was"
    , Space
    , Str "built"
    , Space
    , Str "and"
    , Space
    , Str "maintained"
    , Space
    , Str "at"
    , Space
    , Str "Los"
    , Space
    , Str "Alamos"
    , SoftBreak
    , Str "National"
    , Space
    , Str "Laboratory."
    ]
, Header
    2
    ( "" , [] , [] )
    [ Str "Back"
    , Space
    , Str "matter"
    , Space
    , Str "of"
    , Space
    , Str "book"
    , Space
    , Str "part"
    ]
, Header 1 ( "" , [] , [] ) [ Str "References" ]
, Div ( "refs" , [] , [] ) []
]
```