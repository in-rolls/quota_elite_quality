# Literature table

Source for the appendix table "Published comparisons of leader qualifications by women's reservation".

- `studies/<bibkey>.yaml`: one file per study, keyed to `manuscript/references.bib`. This is the source of truth.
- `tables.yaml`: caption, study order, sections and notes.
- `R/literature.R` reads the studies into one row per (study, outcome) and computes SEs marked `se_source: computed`.
- `scripts/lit_table.R` typesets `output/literature_table.tex`; `make manuscript` runs it. It changes no content.
- `tests/test_literature.R` checks required fields, share ranges, reserved − open arithmetic, the computed-SE formula, and that numbers quoted in the Existing evidence section match these files.

## Study files

`setting` describes the sample; `defaults` apply to every estimate unless the estimate overrides them. Each estimate records:

| field | meaning |
|---|---|
| `population` | `winners` or `candidates` |
| `comparison` | `reserved_vs_open`; `reserved_women_vs_men` (reserved women vs. men in all other seats); `relative_own_gender` (each group benchmarked against citizens of its own gender); `husband_vs_open` |
| `family`, `unit` | outcome family (`years`, `literate`, `illiterate`, `secondary_plus`, `graduate`, `selection_score`, `prior_office`, `first_time`, `knowledge`) and unit (`years`, `share`, `sd`, `index`, `count`) |
| `reserved`, `open` | group means as reported; shares stored as proportions |
| `diff`, `se` | reserved minus open (signs reversed where the source reports open minus reserved) and its SE |
| `se_source` | `reported`, or `computed` from shares and group sizes as for independent samples (ignores clustering) |
| `adjusted` | `true` when `diff` is a regression coefficient rather than a difference in means |
| `sample_group` | studies sharing a sample share this tag, so later pooling does not count them twice |
| `status`, `source`, `note` | audit trail: `confirmed` once checked against the cited source page |

Every value was read from the cited table and then checked by searching the source PDF's text for it. The Beaman et al. group sizes are sums of GP counts given on p. 1506, not a number printed in the table.

## Screened and excluded

| study | reason |
|---|---|
| Besley, Pande and Rao (2005), *Political Selection and the Quality of Government* | Selection of politicians relative to villagers, interacted with reservation; no reserved-vs-open contrast. Same survey as Ban and Rao. |
| Rajaraman and Gupta (2012) | Compares female and male sarpanches, not reserved and open seats. |
| McManus (2014), HKS policy analysis | Compares seats reserved for low-caste women with open seats; mixes caste and gender reservation. |
| Chattopadhyay and Duflo (2003), EPW version | Repeats the West Bengal Table 3 numbers. |
| Brulé, Heinze and Chauchard (2026), MPSA draft | Reports a quota vs. non-quota comparison of sarpanch characteristics, but the draft asks not to be circulated; excluded pending permission. |
| Mori et al. (2025), World Development | Published version of Mori et al. (2022); cited in the text for the relative benchmark, numbers not transcribed. |
