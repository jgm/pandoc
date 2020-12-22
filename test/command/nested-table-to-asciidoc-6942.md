A table within a table should be convertet into a table within table

```
% pandoc -f html -t asciidoc
<!doctype html>
<html>
<head>
<meta charset="utf-8">
<title> NestedTables </title>
</head>
<body>
<table>
 <tr>
  <td >
   <table>  <tr> <td> a1 </td> <td> a2 </td> </tr>  </table>
  </td>
  <td>b</td>
 </tr>
 <tr>
   <td>c</td> <td>d </td>
 </tr>
</table>
</body>
</html>
^D
[width="100%",cols="50%,50%",]
|===
a|
[cols=",",]
!===
!a1 !a2
!===

|b
|c |d
|===
```

A table within a table within a table cannot be converted because asciidoc only supports two levels of tables.
The table on level 3 is thus converted to level 2 and a warning is produced
```
% pandoc -f html -t asciidoc
<!doctype html>
<html>
<head>
<meta charset="utf-8">
<title> NestedTables </title>
</head>
<body>
<table>
 <tr>
  <td>
    <table>  <tr>
	<td> a1 </td>
	<td>
	  <table>  <tr> <td> 1 </td> <td> 2 </td> </tr>  </table>
	</td>
    </tr>  </table>
  </td>
  <td>b</td>
 </tr>
 <tr>
   <td>c</td> <td>d </td>
 </tr>
</table>
</body>
</html>
^D
[WARNING] Target format does not support a nested table from the input because Asciidoc only supports nesting tables up to one level. However, the table in question is nested at level 3. It will be printed at level 1 so no content is lost but will probably need a manual fix by the user.
[width="100%",cols="50%,50%",]
|===
a|
[width="100%",cols="50%,50%",]
!===
!a1 a!
[cols=",",]
!===
!1 !2
!===

!===

|b
|c |d
|===
```
