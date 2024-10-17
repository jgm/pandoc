```
% pandoc -f rst -t native
|figure05|

.. |figure05| figure:: img/dummy.png

   capt
^D
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
