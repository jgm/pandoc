RTL support in the opendocument/odt writer.

`dir: rtl` in metadata should set `style:writing-mode` on paragraph styles:

```
% pandoc -f markdown -t opendocument --template command/odt-rtl/styles.opendocument
---
dir: rtl
---

# Heading

Hello world.

> quoted
^D
<style:style style:name="fr2" style:family="graphic" style:parent-style-name="Formula"><style:graphic-properties style:vertical-pos="middle" style:vertical-rel="text" style:horizontal-pos="center" style:horizontal-rel="paragraph-content" style:wrap="none" /></style:style>
<style:style style:name="fr1" style:family="graphic" style:parent-style-name="Formula"><style:graphic-properties style:vertical-pos="middle" style:vertical-rel="text" /></style:style>
<style:style style:name="P1" style:family="paragraph" style:parent-style-name="Heading_20_1">
  <style:paragraph-properties style:writing-mode="rl-tb" fo:text-align="right" />
</style:style>
<style:style style:name="P2" style:family="paragraph" style:parent-style-name="First_20_paragraph">
  <style:paragraph-properties style:writing-mode="rl-tb" fo:text-align="right" />
</style:style>
<style:style style:name="P3" style:family="paragraph" style:parent-style-name="Quotations">
  <style:paragraph-properties style:writing-mode="rl-tb" fo:text-align="right" />
</style:style>
<text:h text:style-name="P1" text:outline-level="1"><text:bookmark-start text:name="heading" />Heading<text:bookmark-end text:name="heading" /></text:h>
<text:p text:style-name="P2">Hello world.</text:p>
<text:p text:style-name="P3">quoted</text:p>
```

An RTL `lang` in metadata implies RTL direction:

```
% pandoc -f markdown -t opendocument --template command/odt-rtl/styles.opendocument
---
lang: he
---

Hello world.
^D
<style:style style:name="fr2" style:family="graphic" style:parent-style-name="Formula"><style:graphic-properties style:vertical-pos="middle" style:vertical-rel="text" style:horizontal-pos="center" style:horizontal-rel="paragraph-content" style:wrap="none" /></style:style>
<style:style style:name="fr1" style:family="graphic" style:parent-style-name="Formula"><style:graphic-properties style:vertical-pos="middle" style:vertical-rel="text" /></style:style>
<style:style style:name="P1" style:family="paragraph" style:parent-style-name="Text_20_body">
  <style:paragraph-properties style:writing-mode="rl-tb" fo:text-align="right" />
</style:style>
<text:p text:style-name="P1">Hello world.</text:p>
```

`dir: ltr` in metadata overrides an RTL language:

```
% pandoc -f markdown -t opendocument --template command/odt-rtl/styles.opendocument
---
lang: he
dir: ltr
---

Hello world.
^D
<style:style style:name="fr2" style:family="graphic" style:parent-style-name="Formula"><style:graphic-properties style:vertical-pos="middle" style:vertical-rel="text" style:horizontal-pos="center" style:horizontal-rel="paragraph-content" style:wrap="none" /></style:style>
<style:style style:name="fr1" style:family="graphic" style:parent-style-name="Formula"><style:graphic-properties style:vertical-pos="middle" style:vertical-rel="text" /></style:style>
<text:p text:style-name="Text_20_body">Hello world.</text:p>
```

A `dir` attribute on a div changes direction for its contents:

```
% pandoc -f markdown -t opendocument --template command/odt-rtl/styles.opendocument
Plain paragraph.

::: {dir=rtl}
RTL paragraph.
:::

After div.
^D
<style:style style:name="fr2" style:family="graphic" style:parent-style-name="Formula"><style:graphic-properties style:vertical-pos="middle" style:vertical-rel="text" style:horizontal-pos="center" style:horizontal-rel="paragraph-content" style:wrap="none" /></style:style>
<style:style style:name="fr1" style:family="graphic" style:parent-style-name="Formula"><style:graphic-properties style:vertical-pos="middle" style:vertical-rel="text" /></style:style>
<style:style style:name="P1" style:family="paragraph" style:parent-style-name="Text_20_body">
  <style:paragraph-properties style:writing-mode="rl-tb" fo:text-align="right" />
</style:style>
<text:p text:style-name="Text_20_body">Plain paragraph.</text:p>
<text:p text:style-name="P1">RTL paragraph.</text:p>
<text:p text:style-name="Text_20_body">After div.</text:p>
```
