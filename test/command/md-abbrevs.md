Pandoc recognizes an abbreviation and inserts a nonbreaking
space (among other things, this prevents a sentence-ending
space from being inserted in LaTeX output).

```
% pandoc -t native
Mr. Bob
^D
[Para [Str "Mr.\160Bob"]]
```

If you don't want this to happen you can escape the period:

```
% pandoc -t native
Hi Mr\. Bob
^D
[Para [Str "Hi",Space,Str "Mr.",Space,Str "Bob"]]
```

