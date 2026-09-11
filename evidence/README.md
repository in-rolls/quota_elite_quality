# Source excerpt

`bamezai_2026_table_7.png` reproduces the numerical panel of Table 7, page 44, from Bamezai, George, Sharan and Sun, *The Determinants and Consequences of Political Selection in the Developing World: Evidence from India*, dated March 26, 2026. [Source PDF](https://www.mrsharan.com/papers/political-selection.pdf#page=44), retrieved September 10, 2026.

The crop omits the title and notes; the main README supplies attribution, units and a link to the full specification. No values were changed. The authors' PDF is hosted at a URL that may change versions, so its SHA-256 at retrieval was `1ecf95d70e71297c8608bdc16f31c4c17c75e034f73ae07b7efa9c5fa6cf92be`.

To render this excerpt from that PDF with Poppler:

```sh
pdftoppm -f 44 -l 44 -singlefile -r 144 -x 195 -y 493 -W 795 -H 447 -png political-selection.pdf evidence/bamezai_2026_table_7
```

The three earlier table images in the repository root predate this update. Their authors, table numbers and source links appear beside them in the main README.
