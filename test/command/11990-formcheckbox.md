Legacy check box form fields (FORMCHECKBOX) are rendered as task
list characters, reflecting their checked state (#11990).

```
% pandoc command/11990-formcheckbox.docx -t native
^D
[ Para [ Str "\9744" , Space , Str "unchecked" ]
, Para [ Str "\9746" , Space , Str "checked" ]
]
```
