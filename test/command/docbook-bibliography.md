```
% pandoc -f docbook -t native --quiet
<bibliodiv>
<title>Document References</title>
<bibliomixed>
<bibliomisc><anchor xml:id="refTheFirst" xreflabel="[1]"/>[1] First reference</bibliomisc>
</bibliomixed>
<bibliomixed>
<bibliomisc><anchor xml:id="refTheSecond" xreflabel="[2]"/>[2] Second reference</bibliomisc>
</bibliomixed>
<bibliomixed>
<bibliomisc><anchor xml:id="refTheThird" xreflabel="[3]"/>[3] Third reference</bibliomisc>
</bibliomixed>
</bibliodiv>
^D
[ Header 1 ( "" , [] , [] ) [ Str "Document References" ]
, Para
    [ Span ( "refTheFirst" , [] , [] ) []
    , Str "[1] First reference"
    ]
, Para
    [ Span ( "refTheSecond" , [] , [] ) []
    , Str "[2] Second reference"
    ]
, Para
    [ Span ( "refTheThird" , [] , [] ) []
    , Str "[3] Third reference"
    ]
]
```
