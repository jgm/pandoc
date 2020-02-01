```
% pandoc -f csv -t native
Fruit,Price,Quantity
Apple,25 cents,33
"""Navel"" Orange","35 cents",22
^D
[Table [] [AlignDefault,AlignDefault,AlignDefault] [0.0,0.0,0.0]
 [[Plain [Str "Fruit"]]
 ,[Plain [Str "Price"]]
 ,[Plain [Str "Quantity"]]]
 [[[Plain [Str "Apple"]]
  ,[Plain [Str "25",Space,Str "cents"]]
  ,[Plain [Str "33"]]]
 ,[[Plain [Str "\"Navel\"",Space,Str "Orange"]]
  ,[Plain [Str "35",Space,Str "cents"]]
  ,[Plain [Str "22"]]]]]
```
