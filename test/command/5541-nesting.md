```
% pandoc -f html -t icml -s

<div id="blockId">
  <div id="blockId2">
    <span id="inlineId">
      <img id="inlineId2" src="lalune.jpg" />
    </span>
  </div>
</div>

^D
<?xml version="1.0" encoding="UTF-8" standalone="yes"?>
<?aid style="50" type="snippet" readerVersion="6.0" featureSet="513" product="8.0(370)" ?>
<?aid SnippetType="InCopyInterchange"?>
<Document DOMVersion="8.0" Self="pandoc_doc">
    <RootCharacterStyleGroup Self="pandoc_character_styles">
      <CharacterStyle Self="$ID/NormalCharacterStyle" Name="Default" />
      <CharacterStyle Self="CharacterStyle/" Name="">
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
      <ParagraphStyle Self="ParagraphStyle/" Name="" LeftIndent="0">
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
<ParagraphStyleRange AppliedParagraphStyle="">
  <CharacterStyleRange AppliedCharacterStyle="$ID/NormalCharacterStyle">
    <HyperlinkTextDestination Self="HyperlinkTextDestination/#inlineId" Name="Destination" DestinationUniqueKey="1" />
    <Content> </Content>
  </CharacterStyleRange>
  <CharacterStyleRange AppliedCharacterStyle="$ID/NormalCharacterStyle">
    <Rectangle Self="uec" StrokeWeight="0" ItemTransform="1 0 0 1 75 -75">
      <Properties>
        <PathGeometry>
          <GeometryPathType PathOpen="false">
            <PathPointArray>
              <PathPointType Anchor="-75 -75" LeftDirection="-75 -75" RightDirection="-75 -75" />
              <PathPointType Anchor="-75 75" LeftDirection="-75 75" RightDirection="-75 75" />
              <PathPointType Anchor="75 75" LeftDirection="75 75" RightDirection="75 75" />
              <PathPointType Anchor="75 -75" LeftDirection="75 -75" RightDirection="75 -75" />
            </PathPointArray>
          </GeometryPathType>
        </PathGeometry>
      </Properties>
      <Image Self="ue6" ItemTransform="1 0 0 1 -75 -75">
        <Properties>
          <Profile type="string">
            $ID/Embedded
          </Profile>
          <GraphicBounds Left="0" Top="0" Right="150" Bottom="150" />
        </Properties>
        <Link Self="ueb" LinkResourceURI="file:lalune.jpg" />
      </Image>
    </Rectangle>
  </CharacterStyleRange>
  <CharacterStyleRange AppliedCharacterStyle="$ID/NormalCharacterStyle">
    <HyperlinkTextDestination Self="HyperlinkTextDestination/#inlineId" Name="Destination" DestinationUniqueKey="1" />
    <Content> </Content>
  </CharacterStyleRange>
</ParagraphStyleRange>

  </Story>
  
</Document>
```
