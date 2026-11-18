Plain TeX font declarations produce no text.

```
% pandoc -f latex -t native
\font\tenmsb=msbm10
\font\sevenmsb=msbm7 at 7pt
\font\big=cmr10 scaled \magstep2
\newfam\msbfam
\textfont\msbfam=\tenmsb
\scriptfont\msbfam=\sevenmsb
Text \font\y=cmr10 at 10.95pt here.
^D
[ Para [ Str "Text" , Space , Str "here." ] ]
```

```
% pandoc -f latex+raw_tex -t native
\font\tenmsb=msbm10
\font\sevenmsb=msbm7 at 7pt
\font\big=cmr10 scaled \magstep2
\newfam\msbfam
\textfont\msbfam=\tenmsb
\scriptfont\msbfam=\sevenmsb
Text \font\y=cmr10 at 10.95pt here.
^D
[ Para
    [ RawInline (Format "latex") "\\font\\tenmsb=msbm10"
    , SoftBreak
    , RawInline (Format "latex") "\\font\\sevenmsb=msbm7 at 7pt"
    , SoftBreak
    , RawInline
        (Format "latex") "\\font\\big=cmr10 scaled \\magstep2"
    , SoftBreak
    , RawInline (Format "latex") "\\newfam\\msbfam"
    , SoftBreak
    , RawInline (Format "latex") "\\textfont\\msbfam=\\tenmsb"
    , SoftBreak
    , RawInline
        (Format "latex") "\\scriptfont\\msbfam=\\sevenmsb"
    , SoftBreak
    , Str "Text"
    , Space
    , RawInline (Format "latex") "\\font\\y=cmr10 at 10.95pt"
    , Space
    , Str "here."
    ]
]
```
