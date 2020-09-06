```
% pandoc -f markdown -t icml -s

# Header 1

this is some text

## Header 2

some more text that [links to](#header-1) the first header. And this links to [some text](#spanner) in 2.1.

## Header 2.1

if you can read this text, [and it's linked]{#spanner} - all good!

^D
<?xml version="1.0" encoding="UTF-8" standalone="yes"?>
<?aid style="50" type="snippet" readerVersion="6.0" featureSet="513" product="8.0(370)" ?>
<?aid SnippetType="InCopyInterchange"?>
<Document DOMVersion="8.0" Self="pandoc_doc">
    <RootCharacterStyleGroup Self="pandoc_character_styles">
      <CharacterStyle Self="$ID/NormalCharacterStyle" Name="Default" />
      <CharacterStyle Self="CharacterStyle/Link" Name="Link">
        <Properties>
          <BasedOn type="object">$ID/NormalCharacterStyle</BasedOn>
        </Properties>
      </CharacterStyle>
    </RootCharacterStyleGroup>
    <RootParagraphStyleGroup Self="pandoc_paragraph_styles">
      <ParagraphStyle Self="$ID/NormalParagraphStyle" Name="$ID/NormalParagraphStyle"
          SpaceBefore="6" SpaceAfter="6"> <!-- paragraph spacing -->
        <Properties>
          <TabList type="list">
            <ListItem type="record">
              <Alignment type="enumeration">LeftAlign</Alignment>
              <AlignmentCharacter type="string">.</AlignmentCharacter>
              <Leader type="string"></Leader>
              <Position type="unit">10</Position> <!-- first tab stop -->
            </ListItem>
          </TabList>
        </Properties>
      </ParagraphStyle>
      <ParagraphStyle Self="ParagraphStyle/Header1" Name="Header1" LeftIndent="0" PointSize="36">
        <Properties>
          <BasedOn type="object">$ID/NormalParagraphStyle</BasedOn>
        </Properties>
      </ParagraphStyle>
      <ParagraphStyle Self="ParagraphStyle/Header2" Name="Header2" LeftIndent="0" PointSize="30">
        <Properties>
          <BasedOn type="object">$ID/NormalParagraphStyle</BasedOn>
        </Properties>
      </ParagraphStyle>
      <ParagraphStyle Self="ParagraphStyle/Paragraph" Name="Paragraph" LeftIndent="0">
        <Properties>
          <BasedOn type="object">$ID/NormalParagraphStyle</BasedOn>
        </Properties>
      </ParagraphStyle>
    </RootParagraphStyleGroup>
    <RootTableStyleGroup Self="pandoc_table_styles">
      <TableStyle Self="TableStyle/Table" Name="Table" />
    </RootTableStyleGroup>
    <RootCellStyleGroup Self="pandoc_cell_styles">
      <CellStyle Self="CellStyle/Cell" AppliedParagraphStyle="ParagraphStyle/$ID/[No paragraph style]" Name="Cell" />
    </RootCellStyleGroup>
  <Story Self="pandoc_story"
      TrackChanges="false"
      StoryTitle=""
      AppliedTOCStyle="n"
      AppliedNamedGrid="n" >
    <StoryPreference OpticalMarginAlignment="true" OpticalMarginSize="12" />

<!-- body needs to be non-indented, otherwise code blocks are indented too far -->
<ParagraphStyleRange AppliedParagraphStyle="ParagraphStyle/Header1">
  <CharacterStyleRange AppliedCharacterStyle="$ID/NormalCharacterStyle">
    <HyperlinkTextDestination Self="HyperlinkTextDestination/#header-1" Name="Destination" DestinationUniqueKey="1" />
    <Content>Header 1</Content>
  </CharacterStyleRange>
</ParagraphStyleRange>
<Br />
<ParagraphStyleRange AppliedParagraphStyle="ParagraphStyle/Paragraph">
  <CharacterStyleRange AppliedCharacterStyle="$ID/NormalCharacterStyle">
    <Content>this is some text</Content>
  </CharacterStyleRange>
</ParagraphStyleRange>
<Br />
<ParagraphStyleRange AppliedParagraphStyle="ParagraphStyle/Header2">
  <CharacterStyleRange AppliedCharacterStyle="$ID/NormalCharacterStyle">
    <HyperlinkTextDestination Self="HyperlinkTextDestination/#header-2" Name="Destination" DestinationUniqueKey="1" />
    <Content>Header 2</Content>
  </CharacterStyleRange>
</ParagraphStyleRange>
<Br />
<ParagraphStyleRange AppliedParagraphStyle="ParagraphStyle/Paragraph">
  <CharacterStyleRange AppliedCharacterStyle="$ID/NormalCharacterStyle">
    <Content>some more text that </Content>
  </CharacterStyleRange>
  <HyperlinkTextSource Self="htss-1" Name="" Hidden="false">
    <CharacterStyleRange AppliedCharacterStyle="CharacterStyle/Link">
      <Content>links to</Content>
    </CharacterStyleRange>
  </HyperlinkTextSource>
  <CharacterStyleRange AppliedCharacterStyle="$ID/NormalCharacterStyle">
    <Content> the first header. And this links to </Content>
  </CharacterStyleRange>
  <HyperlinkTextSource Self="htss-2" Name="" Hidden="false">
    <CharacterStyleRange AppliedCharacterStyle="CharacterStyle/Link">
      <Content>some text</Content>
    </CharacterStyleRange>
  </HyperlinkTextSource>
  <CharacterStyleRange AppliedCharacterStyle="$ID/NormalCharacterStyle">
    <Content> in 2.1.</Content>
  </CharacterStyleRange>
</ParagraphStyleRange>
<Br />
<ParagraphStyleRange AppliedParagraphStyle="ParagraphStyle/Header2">
  <CharacterStyleRange AppliedCharacterStyle="$ID/NormalCharacterStyle">
    <HyperlinkTextDestination Self="HyperlinkTextDestination/#header-2.1" Name="Destination" DestinationUniqueKey="1" />
    <Content>Header 2.1</Content>
  </CharacterStyleRange>
</ParagraphStyleRange>
<Br />
<ParagraphStyleRange AppliedParagraphStyle="ParagraphStyle/Paragraph">
  <CharacterStyleRange AppliedCharacterStyle="$ID/NormalCharacterStyle">
    <Content>if you can read this text, </Content>
  </CharacterStyleRange>
  <CharacterStyleRange AppliedCharacterStyle="$ID/NormalCharacterStyle">
    <HyperlinkTextDestination Self="HyperlinkTextDestination/#spanner" Name="Destination" DestinationUniqueKey="1" />
    <Content>and it’s linked</Content>
  </CharacterStyleRange>
  <CharacterStyleRange AppliedCharacterStyle="$ID/NormalCharacterStyle">
    <Content> - all good!</Content>
  </CharacterStyleRange>
</ParagraphStyleRange>

  </Story>
  <Hyperlink Self="uf-2" Name="#spanner" Source="htss-2" Visible="false" DestinationUniqueKey="1">
    <Properties>
      <BorderColor type="enumeration">Black</BorderColor>
      <Destination type="object">HyperlinkTextDestination/#spanner</Destination>
    </Properties>
  </Hyperlink>
  <Hyperlink Self="uf-1" Name="#header-1" Source="htss-1" Visible="false" DestinationUniqueKey="1">
    <Properties>
      <BorderColor type="enumeration">Black</BorderColor>
      <Destination type="object">HyperlinkTextDestination/#header-1</Destination>
    </Properties>
  </Hyperlink>
</Document>
```
