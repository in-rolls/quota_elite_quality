# Evidence guide

This directory connects the manuscript's reported estimates and claims to their sources and audit records.

| Material | Where to look |
|---|---|
| Published comparisons used in the literature table and meta-analysis | [Literature guide](literature/README.md), [study estimates](literature/studies/), and [table specification](literature/tables.yaml) |
| Original table and page excerpts | [Excerpt images](excerpts/) |
| Source URLs, versions, table locations, and excerpt hashes | [Source registry](sources.csv) |
| Analysis input hashes and upstream revisions | [Input manifest](analysis_inputs.csv) and [adapter diff](central_adapters.patch) |
| Data coverage and use | [Data inventory](data_inventory.csv) |
| Claims, supporting artifacts, and limitations | [Claim ledger](claim_ledger.csv) and [audit checks](audit_checks.csv) |
| Councillor deposit comparisons | [Deposit audit](deposit_audit.md) |
| Earlier descriptive tables and education-label corrections | [Historical tables](legacy_readme_tables.json) and [Kerala corrections](kerala_label_corrections.csv) |

Study YAML files hold the transcribed estimates. Their `key` matches `citation` in the source registry and the citation key in the [bibliography](../manuscript/references.bib). A source can have several registry rows for different excerpts; the registry also covers sources outside the literature table.

The registry's `artifact` paths are relative to this directory: `excerpts/` holds images, and `../output/` points to generated audits. Blank artifact fields identify sources without a local artifact. Recorded SHA-256 hashes verify the excerpt files. Downloaded source PDFs, when available locally, remain in the ignored `sources/` cache.

Run `make test` from the repository root to check study-to-registry coverage, artifact paths, recorded hashes, and the literature estimates. Generated tables, figures, and analyses live in [output](../output/); build commands are in the [project README](../README.md#reproduce).
