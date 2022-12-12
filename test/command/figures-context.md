# Figure with one image, caption and label

```
% pandoc -t context -f html
<figure>
  <img src="mandrill.jpg" />
  <figcaption><q>The Mandrill</q>, a photo used in
    image processing tests.</figcaption>
</figure>
^D
\startplacefigure[title={\quotation{The Mandrill}, a photo used in image
processing tests.}]
{\externalfigure[mandrill.jpg]}
\stopplacefigure
```

# Nested figures

```
% pandoc -t context -f html
<figure id="test-images">
  <figure id="mandrill">
    <img src="../testing/mandrill.jpg">
    <figcaption><q>The Mandrill</q> is a commonly used test image.</figcaption>
  </figure>
  <figure id="peppers">
    <img src="../testing/peppers.webp" >
    <figcaption>Another test image. This one is called <q>peppers</q>.</figcaption>
  </figure>
  <figcaption>Signal processing test images.</figcaption>
</figure>
^D
\startplacefigure[reference=test-images,title={Signal processing test
images.}]
\startfloatcombination
\startplacefigure[reference=mandrill,title={\quotation{The Mandrill} is
a commonly used test image.}]
{\externalfigure[../testing/mandrill.jpg]}
\stopplacefigure

\startplacefigure[reference=peppers,title={Another test image. This one
is called \quotation{peppers}.}]
{\externalfigure[../testing/peppers.webp]}
\stopplacefigure

\stopfloatcombination
\stopplacefigure
```
