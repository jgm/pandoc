```
% pandoc -f dokuwiki -t native
> <code>some
code
</code>
>> ok
> then
> more
^D
[ BlockQuote
    [ CodeBlock ( "" , [] , [] ) "some\ncode\n"
    , BlockQuote [ Plain [ Str "ok" ] ]
    , Plain [ Str "then" , LineBreak , Str "more" ]
    ]
]
```
