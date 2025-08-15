```
% pandoc -t gfm
- list item
  <details>

  - subitem
  </details>

  item _continue_ **with** formatting
- next list item
^D
- list item
  <details>

  - subitem

  </details>

  item *continue* **with** formatting
- next list item
```
