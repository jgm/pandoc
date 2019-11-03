```
% pandoc -t opendocument+native_numbering
  Right     Left  
-------     ------
     12     11

: First table

  Right     Left  
-------     ------
     13     14    

: Second Table
^D
<text:p text:style-name="TableCaption">Table <text:sequence text:ref-name="refTable0" text:name="Table" text:formula="ooow:Table+1" style:num-format="1">1</text:sequence>: First
table</text:p>
<table:table table:name="Table1" table:style-name="Table1">
  <table:table-column table:style-name="Table1.A" />
  <table:table-column table:style-name="Table1.B" />
  <table:table-header-rows>
    <table:table-row>
      <table:table-cell table:style-name="TableHeaderRowCell" office:value-type="string">
        <text:p text:style-name="P1">Right</text:p>
      </table:table-cell>
      <table:table-cell table:style-name="TableHeaderRowCell" office:value-type="string">
        <text:p text:style-name="Table_20_Heading">Left</text:p>
      </table:table-cell>
    </table:table-row>
  </table:table-header-rows>
  <table:table-row>
    <table:table-cell table:style-name="TableRowCell" office:value-type="string">
      <text:p text:style-name="P2">12</text:p>
    </table:table-cell>
    <table:table-cell table:style-name="TableRowCell" office:value-type="string">
      <text:p text:style-name="Table_20_Contents">11</text:p>
    </table:table-cell>
  </table:table-row>
</table:table>
<text:p text:style-name="TableCaption">Table <text:sequence text:ref-name="refTable1" text:name="Table" text:formula="ooow:Table+1" style:num-format="1">2</text:sequence>: Second
Table</text:p>
<table:table table:name="Table2" table:style-name="Table2">
  <table:table-column table:style-name="Table2.A" />
  <table:table-column table:style-name="Table2.B" />
  <table:table-header-rows>
    <table:table-row>
      <table:table-cell table:style-name="TableHeaderRowCell" office:value-type="string">
        <text:p text:style-name="P3">Right</text:p>
      </table:table-cell>
      <table:table-cell table:style-name="TableHeaderRowCell" office:value-type="string">
        <text:p text:style-name="Table_20_Heading">Left</text:p>
      </table:table-cell>
    </table:table-row>
  </table:table-header-rows>
  <table:table-row>
    <table:table-cell table:style-name="TableRowCell" office:value-type="string">
      <text:p text:style-name="P4">13</text:p>
    </table:table-cell>
    <table:table-cell table:style-name="TableRowCell" office:value-type="string">
      <text:p text:style-name="Table_20_Contents">14</text:p>
    </table:table-cell>
  </table:table-row>
</table:table>
```
