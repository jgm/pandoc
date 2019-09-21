```
% pandoc -t opendocument+native_numbering

![First image](lalune.jpg)

![Second image](lalune.jpg)

^D
<text:p text:style-name="FigureWithCaption"><draw:frame draw:name="img1"><draw:image xlink:href="lalune.jpg" xlink:type="simple" xlink:show="embed" xlink:actuate="onLoad" /></draw:frame></text:p>
<text:p text:style-name="FigureCaption">Figure <text:sequence text:ref-name="refIllustration0" text:name="Illustration" text:formula="ooow:Illustration+1" style:num-format="1">1</text:sequence>: First
image</text:p>
<text:p text:style-name="FigureWithCaption"><draw:frame draw:name="img2"><draw:image xlink:href="lalune.jpg" xlink:type="simple" xlink:show="embed" xlink:actuate="onLoad" /></draw:frame></text:p>
<text:p text:style-name="FigureCaption">Figure <text:sequence text:ref-name="refIllustration1" text:name="Illustration" text:formula="ooow:Illustration+1" style:num-format="1">2</text:sequence>: Second
image</text:p>
```
