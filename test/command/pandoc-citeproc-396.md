```
% pandoc -t markdown-citations
---
csl: command/chicago-annotated-bibliography.csl
references:
- author:
  - family: Lenz
    given: Jakob Michael Reinhold
  container-author:
  - family: Lenz
    given: Jakob Michael Reinhold
  container-title: Werke und briefe in drei bänden
  editor:
  - family: Damm
    given: Sigrid
  id: Lenz
  issued: 1992
  page: '198-204'
  title: Tantalus
  type: chapter
  volume: 3
---

This is about Lenz' "Tantalus". [@Lenz] Note how the "edited by"
in the bibliography is not capitalised even though it follows a
full stop.
^D
This is about Lenz' "Tantalus."[^1] Note how the "edited by" in the
bibliography is not capitalised even though it follows a full stop.

::: {#refs .references .hanging-indent}
::: {#ref-Lenz}
Lenz, Jakob Michael Reinhold. "Tantalus." In *Werke Und Briefe in Drei
bänden*, by Jakob Michael Reinhold Lenz, 198--204. Edited by Sigrid Damm,
1992.
:::
:::

[^1]: Lenz, "Tantalus."
```
