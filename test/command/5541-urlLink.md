```
% pandoc -f markdown -t icml -s

# Header 1

this is some text

## Header 2

some more text that [links to](https://www.pandoc.org) Pandoc.

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
    <Content> Pandoc.</Content>
  </CharacterStyleRange>
</ParagraphStyleRange>

  </Story>
  <HyperlinkURLDestination Self="HyperlinkURLDestination/https%3a//www.pandoc.org" Name="link" DestinationURL="https://www.pandoc.org" DestinationUniqueKey="1" />
  <Hyperlink Self="uf-1" Name="https://www.pandoc.org" Source="htss-1" Visible="true" DestinationUniqueKey="1">
    <Properties>
      <BorderColor type="enumeration">Black</BorderColor>
      <Destination type="object">HyperlinkURLDestination/https%3a//www.pandoc.org</Destination>
    </Properties>
  </Hyperlink>
</Document>
```