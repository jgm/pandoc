```
% pandoc -f jats -t native
<book dtd-version="2.1" xmlns:xlink="http://www.w3.org/1999/xlink" xmlns:mml="http://www.w3.org/1998/Math/MathML">
 	<front-matter>
  		<front-matter-part>
   			<book-part-meta>
    			<title-group>
     				<title>About this book</title>
    			</title-group>
   			</book-part-meta>
   			<named-book-part-body>
    			<sec sec-type="miscinfo">
     				<title>The NCBI Handbook</title>
     				<p>Bioinformatics consists of a computational approach
      				to biomedical information management and analysis.</p>
    			</sec>
   			</named-book-part-body>
  		</front-matter-part>
 	</front-matter>
 	<book-body>
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
 	</book-body>
 	<book-back>
  		<ack id="bid.394">
   		<title>Acknowledgments</title>
   		<p>We gratefully acknowledge the work of Vladimir Soussov,
    	as well as the entire NCBI Entrez team...</p>
  		</ack>
 	</book-back>
</book>
^D
[ Header
    2
    ( "" , [] , [] )
    [ Str "The" , Space , Str "NCBI" , Space , Str "Handbook" ]
, Para
    [ Str "Bioinformatics"
    , Space
    , Str "consists"
    , Space
    , Str "of"
    , Space
    , Str "a"
    , Space
    , Str "computational"
    , Space
    , Str "approach"
    , SoftBreak
    , Str "to"
    , Space
    , Str "biomedical"
    , Space
    , Str "information"
    , Space
    , Str "management"
    , Space
    , Str "and"
    , Space
    , Str "analysis."
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
, Header 2 ( "bid.394" , [] , [] ) [ Str "Acknowledgments" ]
, Para
    [ Str "We"
    , Space
    , Str "gratefully"
    , Space
    , Str "acknowledge"
    , Space
    , Str "the"
    , Space
    , Str "work"
    , Space
    , Str "of"
    , Space
    , Str "Vladimir"
    , Space
    , Str "Soussov,"
    , SoftBreak
    , Str "as"
    , Space
    , Str "well"
    , Space
    , Str "as"
    , Space
    , Str "the"
    , Space
    , Str "entire"
    , Space
    , Str "NCBI"
    , Space
    , Str "Entrez"
    , Space
    , Str "team..."
    ]
]
```