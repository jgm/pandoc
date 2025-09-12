obsidian tests:

```
% pandoc -f obsidian -t native
%% a comment %%
^D
[]
```

```
% pandoc -f obsidian -t native
A paragraph with a block ID. ^my-id
^D
[ Div ( "my-id" , [] , [] ) [ Para [ Str "A" , Space , Str "paragraph" , Space , Str "with" , Space , Str "a" , Space , Str "block" , Space , Str "ID." ] ] ]
```

```
% pandoc -f obsidian -t native
An embed: ![[myfile.md]]
^D
[ Para [ Str "An" , Space , Str "embed:" , Space ] ]
```

```
% pandoc -f obsidian -t native
A block reference embed: ![[myfile.md#^my-id]]
^D
[ Para [ Str "A" , Space , Str "block" , Space , Str "reference" , Space , Str "embed:" , Space ] ]
```

```
% pandoc -f obsidian -t native
A wikilink transclusion: ![[my-wikilink]]
^D
[ Para [ Str "A" , Space , Str "wikilink" , Space , Str "transclusion:" , Space ] ]
```

```
% pandoc -f obsidian -t native
A block transclusion: ![[myfile#^my-id]]
^D
[ Para [ Str "A" , Space , Str "block" , Space , Str "transclusion:" , Space ] ]
```

```
% pandoc -f obsidian-wikilink_transclusion -t native
A wikilink transclusion: ![[my-wikilink]]
^D
[ Para [ Str "A" , Space , Str "wikilink" , Space , Str "transclusion:" , Space , Str "!" , Link ( "" , [ "wikilink" ] , [] ) [ Str "my-wikilink" ] ( "my-wikilink" , "" ) ] ]
```

```
% pandoc -f obsidian-block_transclusion -t native
A block transclusion: ![[myfile#^my-id]]
^D
[ Para [ Str "A" , Space , Str "block" , Space , Str "transclusion:" , Space , Str "!" , Link ( "" , [ "wikilink" ] , [] ) [ Str "myfile#^my-id" ] ( "myfile#^my-id" , "" ) ] ]
```

