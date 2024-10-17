```
% pandoc -f rst -t native
|figure05|

.. |figure05| figure:: img/dummy.png

   capt
[ Figure
    ( "" , [] , [] )
    (Caption Nothing [ Plain [ Str "capt" ] ])
    [ Plain
        [ Image
            ( "" , [] , [] )
            [ Str "img/dummy.png" ]
            ( "img/dummy.png" , "" )
        ]
    ]
]
```
