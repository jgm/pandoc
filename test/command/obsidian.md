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
[ Para [ Str "An" , Space , Str "embed:" , Space , Image ( "" , [ "wikilink" ] , [] ) [ Str "myfile.md" ] ( "myfile.md" , "" ) ] ]
```

```
% pandoc -f obsidian -t native
A block reference embed: ![[myfile.md#^my-id]]
^D
[ Para [ Str "A" , Space , Str "block" , Space , Str "reference" , Space , Str "embed:" , Space , Image ( "" , [ "wikilink" ] , [] ) [ Str "myfile.md#^my-id" ] ( "myfile.md#^my-id" , "" ) ] ]
```

