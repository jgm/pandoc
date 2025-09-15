# Online vim documentation

While vim documentation can be accessed with `:help`, it may be beneficial to
link to the websites inside readme/markdown docs. Two most common websites are:

- <https://vimhelp.org/> and
- <https://neovim.io/doc/user>

Also it is not uncommon to reference documentation as `:h <topic>`

## Links to vimhelp.org {#vimhelp-links}

For introduction to writing help files see
<https://vimhelp.org/helphelp.txt.html#help-writing>

Named link: [vimhelp link](https://vimhelp.org/helphelp.txt.html#help-writing)

## Links to neo.vimhelp.org {#neo-vimhelp-links}

For introduction to writing help files see
<https://neo.vimhelp.org/helphelp.txt.html#help-writing>

Named link: [vimhelp link](https://neo.vimhelp.org/helphelp.txt.html#help-writing)

## Links to neovim.io {#neovim-io-links}

For introduction to writing help files see
<https://neovim.io/doc/user/helphelp.html#help-writing>

Named link: [neovim.io link](https://neovim.io/doc/user/helphelp.html#help-writing)

You can also reference whole files with topic omitted:
<https://neovim.io/doc/user/vim_diff.html>

## :h-style documentation {#colon-h-docs}

For introduction to writing help files see
`:h help-writing`

Long name for `:help` is also supported: `:help help-writing`

This is malformed: `:help help-writing `, but this is not: `:help  help-writing`
