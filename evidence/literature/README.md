# Literature review and extracted comparisons

Source for the appendix table "Published comparisons of leader qualifications".

- [Study files](studies/): one `<bibkey>.yaml` per study, keyed to the [bibliography](../../manuscript/references.bib). This is the source of truth for transcribed estimates.
- [Table specification](tables.yaml): caption, study order, sections and notes.
- [Literature reader](../../R/literature.R) reads the studies into one row per (study, outcome) and computes SEs marked `se_source: computed`.
- [Table script](../../R/paper.R) typesets [the appendix table](../../tabs/literature_table.tex); `make manuscript` runs it. It changes no content.
- [Literature tests](../../tests/test_literature.R) check required fields, share ranges, reserved − open arithmetic, the computed-SE formula, source-registry coverage, artifact paths and hashes.

The [source registry](../sources.csv) records source URLs, versions, table locations, and hashes for the [excerpt images](../excerpts/). Its `citation` field matches each study's `key`. See the [evidence guide](../README.md) for project audits and input provenance.

## Study files

`setting` describes the sample; `defaults` apply to every estimate unless the estimate overrides them. Each estimate records:

| field | meaning |
|---|---|
| `population` | `winners` or `candidates` |
| `comparison` | `reserved_vs_open`; `reserved_women_vs_men` (reserved women vs. men in all other seats); `relative_own_gender` (each group benchmarked against citizens of its own gender); `husband_vs_open`; `female_vs_male` (gender comparison, irrespective of seat type) |
| `family`, `unit` | outcome family (`years`, `literate`, `illiterate`, `secondary_plus`, `graduate`, `selection_score`, `prior_office`, `first_time`, `knowledge`, `criminal`) and unit (`years`, `share`, `sd`, `index`, `count`) |
| `reserved`, `open` | group means as reported; shares stored as proportions |
| `diff`, `se` | difference in the direction defined by `comparison` and its SE; normally reserved minus open, with source signs reversed when necessary |
| `se_source` | `reported`; `computed` from shares and group sizes as for independent samples (ignores clustering); or `from_t`, the difference divided by the reported `t` |
| `adjusted` | `true` when `diff` is a regression coefficient rather than a difference in means |
| `sample_group` | studies sharing a sample share this tag, so later pooling does not count them twice |
| `status`, `source`, `note` | audit trail: `confirmed` once checked against the cited source page |

Every value was read from the cited table and then checked by searching the source PDF's text for it. The Beaman et al. group sizes are sums of GP counts given on p. 1506, not a number printed in the table.

## Screened and excluded

| study | reason |
|---|---|
| Besley, Pande and Rao (2005), *Political Selection and the Quality of Government* | Selection of politicians relative to villagers, interacted with reservation; no reserved-vs-open contrast. Same survey as Ban and Rao. |
| Rajaraman and Gupta (2012) | Reports expenditure responses to sarpanch reservation, not an extractable comparison of leader qualifications. The paper in the spending folder is the 2012 article, not the 2008 report named in its record. |
| McManus (2014), HKS policy analysis | Compares seats reserved for low-caste women with open seats; mixes caste and gender reservation. |
| Chattopadhyay and Duflo (2003), EPW version | Repeats the West Bengal Table 3 numbers. |
| Brulé, Heinze and Chauchard (2026), MPSA draft | Reports a quota vs. non-quota comparison of sarpanch characteristics, but the draft asks not to be circulated; excluded pending permission. |
| Mori et al. (2025), World Development | Published version of Mori et al. (2022); cited in the text for the relative benchmark, numbers not transcribed. |

## Review of the spending repository (October 2026)

We screened the 22 study records and 32 local PDFs in `../quota_spending/lit`, then read the relevant sections and tables of the papers below. PDFs include supplements, earlier versions, and conference materials; those counts are not independent studies. The spending review is a discovery aid. Its transcriptions and version labels are not automatically treated as verified findings.

The paper's outcome is **officials' qualifications**. Differences in the eligible population, entry into candidacy, and selection by voters are possible mechanisms. Relative-to-population comparisons inform those mechanisms; they do not replace the reserved-versus-open qualification comparison. Performance and authority studies clarify what qualification measures can and cannot establish.

### Closest articles and organization

| Article consulted | Why it is close | Organization and writing lesson |
|---|---|---|
| [Mori et al. (2025), World Development](https://doi.org/10.1016/j.worlddev.2024.106911) | Quotas, candidate qualifications, and selection in Karnataka. | The accessible 2022 manuscript moves from the question and competing explanations to institutions/data, distributional and regression comparisons, elections, and women's voice. Define the qualification outcome before discussing candidate selection as a mechanism. Published abstract/introduction checked; full published organization not verified. |
| [Gajwani and Zhang (2015), World Bank Economic Review](https://doi.org/10.1093/wber/lhu001) | Schooling, procedural knowledge, and public performance of quota-elected village presidents. | Introduction places the question within conflicting evidence; institutions and assignment precede data/identification; knowledge and contact evidence precede provision results. Keep schooling, practical knowledge, and performance conceptually distinct. Published full text checked. |
| [Cassan and Vandewalle (2021), World Development](https://doi.org/10.1016/j.worlddev.2021.105408) | Reservation changes who holds office, with different implications across social groups. | Introduction states the gap, findings, and contribution; context/data precede separate analyses of descriptive and substantive representation, followed by robustness and conclusion. Organize by the empirical question and distinguish composition from its consequences. Published full text checked. |

Two close articles appear in World Development, which supports using it as the provisional target; that is a judgment about substantive fit, not a prediction of acceptance. Iyer and Mani (2019), also in World Development, supplies an additional published example of defining an outcome and then examining possible explanations. The introduction moves from the importance of qualifications to prior findings, the unresolved puzzle, our contribution, data and research design, and results. The paper then follows related evidence and possible mechanisms → institutions/data/estimation → results → discussion. Rural and urban offices remain separate within the results.

### Additions and qualification-relevant findings

| Study | Verified evidence | Use and limitation |
|---|---|---|
| Gajwani and Zhang (2015) | Schooling distributions, p. 246; knowledge, Table 5 p. 248; contact, Table 6 p. 249. The fully adjusted female coefficient is −3.199 (robust SE 0.474), N = 141, on a 19-item knowledge test. | Added to the review and comparison appendix. Knowledge is not schooling; female status differs from reservation for one GP. The reported schooling percentages are by gender, with no exact reserved/open breakdown. Neither result enters the schooling pool. Covariates include potentially post-reservation characteristics, so the knowledge coefficient is a conditional comparison. |
| Priebe (2017) / Sathe et al. (2013) | Same 2008 Sangli survey of 32 sarpanches. Priebe Table 2 reports schooling means 8.00/13.28 for women/men; Sathe Table 2 reports 8.0/11.3. Priebe note 6 says one of 16 female leaders held a currently unreserved seat. | Added narrative evidence of lower female schooling and different assets. No new pooled estimate: gender comparison, overlapping sample, and unresolved disagreement over the male schooling mean. Priebe supplies rounded p-values, not the SE needed to resolve that discrepancy. |
| Cassan and Vandewalle (2021) | Tables 1–2 link women's reservation to caste composition of candidates/winners; Tables 9–11 examine mobility and political participation. | Added as evidence on possible selection mechanisms. Shares the REDS survey family with Deininger; not an independent schooling sample. |
| Iyer and Mani (2019) | Published introduction and Sections 4–5: citizen participation gaps persist after accounting for observed skills and constraints. | Added to the mechanisms discussion. Citizens' participation is not elected officials' qualifications; adjusted associations do not identify every barrier. |
| Duflo and Topalova (2004) | Tables 2–4 contrast service availability, reported bribes, and satisfaction. | Added to explain why qualifications and evaluations cannot stand in for performance. Millennium Survey results overlap the Beaman et al. (2011) chapter. |
| Beaman et al. (2012) | Tables 1–3 report aspirations and adolescent education; Table S5 examines alternative pathways. | Added as a longer-run consequence of exposure to leaders. These are adolescents' outcomes, not leaders' qualifications. Same Birbhum research setting; not a new schooling sample. |
| Chattopadhyay and Duflo (2004) | Published Table V, public-goods provision in West Bengal and Rajasthan. | Added a separate published citation for performance. The existing leader-characteristics extraction remains tied to the 2001 working paper; tables are not interchangeable across versions. |
| Huidobro, Prillaman and Singhania, *Family Politics* | Public author-site abstract reports shared family governance in male- and female-led councils and lower authority for women. | Public abstract cited for that qualitative distinction only. No numerical estimate or sample count imported from the restricted April 2026 draft. |
| Ban and Rao; Afridi et al.; Deininger et al.; Beaman et al. (2009) | Our existing extracts already include political knowledge or prior experience. Afridi's 2013 working paper studies learning using audit reports. | Expanded their role in the prose beyond schooling. Reused existing verified study records rather than adding duplicate files. |

### Remaining spending-study records

| Spending record | Decision for this paper |
|---|---|
| Ban–Rao; Chattopadhyay–Duflo; Raabe–Sekher–Birner | Already represented in the qualification extracts. The spending outcomes do not add an independent qualification sample. |
| Beaman et al. (2011) | Chapter combines Millennium Survey and Birbhum results. Do not count it independently of Duflo–Topalova or the existing Birbhum studies. |
| Bhalotra–Clots-Figueras (2014) | Spending record is empty and has no linked local PDF. Not treated as reviewed evidence on qualifications. |
| Bardhan–Mookherjee–Parra (2005; 2010) | Different windows/outcomes using the same West Bengal village sample. Studies of targeting, not additional measured schooling comparisons; contextual rather than central here. |
| Bose–Das (2018) | Local PDF is a preliminary September 2014 draft; spending record documents differences from the published findings. Public-works outcomes, no new qualification contrast extracted. |
| Brown–Mitra–Singh (2025) | Groundwater-dependent welfare effects. Useful for a spending review, but not direct evidence on officeholders' qualifications. |
| Chaturvedi–Das–Mahajan (2025) | Local PDF is the September 2023 draft. Toilet allocation and heterogeneous quota effects, not a new qualification comparison. |
| Das (2015), town-hall meetings | Participation and expression of preferences. Does not supply a new winner-qualification estimate. |
| Deininger–Jin–Nagarajan (2012) | Earlier/later author versions overlap the WPS 5708 project already in our extracts. Do not double-count; retain the precisely identified 2011 table. |
| Deininger–Nagarajan–Singh (2020) | Spending record mixes a JCE article with WPS 9350 on later labor-supply effects. Household-head education in WPS 9350 Table 2 is not pradhan education. No qualification result imported. |
| Nilekani (2010) | Karnataka governance indicators and nonrandom reservation assignment; undergraduate thesis. Useful caution on design, not an additional measured qualification contrast. |
| Pakhtigian–Pattanayak (2024) | Local PDF is only a publisher-page capture; concerns citizens' sanitation preferences. No leader-qualification evidence extracted. |
| Pathak–Macours (2017) | Child development and learning following reservation exposure. Distinct outcome from officeholders' qualifications; Beaman et al. (2012) provides the compact example used in the text. |
| Priebe (2017); Sathe et al. (2013) | Same sample; schooling discrepancy and gender-versus-reservation distinction documented above. |
| Rajaraman–Gupta (2012) | Water-spending outcomes; cited year/version in spending record needs care. Not a new leader-qualification comparison. |
| Cassan–Vandewalle; Gajwani–Zhang; Duflo–Topalova | Included above, using inspected source versions. |

### Other PDFs in the spending folder

The two Besley PDFs study political selection and public-good allocation in South India; their survey overlaps Ban–Rao. The selection paper is already listed above as outside the reserved/open synthesis. Ghani–Kerr–O'Connell concerns women's entrepreneurship, Goyal's *Representation from Below* concerns party activism, and Hessami–Lopes da Fonseca is a review of policy effects. They help locate mechanisms or downstream outcomes, but do not supply additional independent qualification estimates for this synthesis.

The Goyal–Mohapatra April 2026 draft concerns spousal succession across elections. Its narrow succession measure cannot establish the prevalence of family involvement during a term. It is retained as a lead, not added as settled evidence. The Bamezai/Amar/Kumar conference slides and Brulé/Heinze/Chauchard deliberation draft explicitly restrict citation or circulation; no new estimates from them are incorporated. Existing public work by Amar et al. and Heinze et al. remains cited. The Beaman supplement and the two Deininger versions are supporting/version material, not separate studies.

That review initially retained the five-sample schooling synthesis and the administrative estimates. The combined synthesis described below supersedes that pooling decision. The new quantitative extraction is Gajwani–Zhang's knowledge coefficient, displayed separately. This was a review of the neighboring repository and targeted source checks, not a systematic search of all published research.

## Combined schooling synthesis

The combined pool uses Chattopadhyay–Duflo (1998 term), Ban–Rao (2002 survey), Afridi et al. (2006–10 term), Deininger et al. (elections observed in REDS 2007), and twelve estimates from our six settings. One Birbhum extraction enters at a time. Repeated administrative elections share setting-level random effects. Years of schooling use the pooled within-leader SD; thresholds use the latent-logistic conversion of the log odds ratio. Our logit models preserve the controls and geographic clustering of the percentage-point analysis. These are heterogeneous qualification comparisons, not a common causal effect or an estimate for every Indian local office.

Bamezai et al. (March 26, 2026 draft), Table 7 Panel B column 1, is a fuzzy-RD estimate near reservation thresholds, in SDs of GP citizens, for 2016 and 2021 winners. Column 2 instead uses own-gender citizens and column 3 substitutes husbands. None is interchangeable with a broad leader-reference schooling difference. We retain the reported evidence in the comparison table but exclude it from every pool; our Bihar 2016 estimate supplies Bihar once. Table 7 names GP fixed effects while equation (9) names block effects; that inconsistency remains unresolved. The prior five-sample pooled SD estimate incorrectly treated the citizen reference as interchangeable and is retired.

The combined forest identifies study publication/draft years and election or survey years separately. The main pool excludes Bamezai; swapping the Birbhum study, excluding Rajasthan, allowing sampling correlation of 0.5 within settings, and leaving each series out are computed in memory. The appendix reports the first three sensitivity checks and the range of leave-one-series-out estimates. The main text and forest plot emphasize separate group summaries: published village heads, our northern village heads, and our Kerala ward members and city councillors. The overall average is retained only in the appendix because it obscures the contrast. This is a descriptive grouping, not an identified mixture distribution or a causal test of urbanization or regional differences. No analysis datasets or result CSVs are written.

## Bhavnani–Huidobro–Prillaman: data search and recoverable contrasts

Checked October 1, 2026: [Bhavnani's research page](https://rikhilbhavnani.com/research.html), [Huidobro's page](https://www.albahuidobro.com/), [Prillaman's research page](https://www.soledadprillaman.com/research), the full public draft and appendix, public repositories of GitHub user `rbhavnani`, and title/coauthor searches restricted to GitHub, Dataverse, OSF and Zenodo. No replication package for this paper was located. The direct Harvard Dataverse catalog queries returned HTTP 403, so those queries could not establish catalog coverage. The current author-site PDF and the github.io PDF are byte-identical (SHA-256 `2c4825e3539f916430ab623d95cfecc8d833d82bb194f19ecd0dcb46d870f80b`). This is a search result, not a claim that the authors have no shareable data.

The October 23, 2024 draft, Table 2 columns 4 and 6, is the relevant reservation specification. Identity columns cannot substitute for reservation. The Odisha comparison is the 2022 election, matched to schooling in the 2011–12 SECC. The 13-state sample is the 2014–16 REDS listing survey; no common election year is specified. Schooling SDs among the relevant winner groups are not tabulated. Figure 1 shows distributions by politician identity, so digitizing it would not supply the reservation-group SDs.

The six coefficients and SEs are transcribed in `studies/bhavnani.yaml`. `bhavnani_contrasts()` computes within-category women's-reservation differences: non-SC/ST women versus fully open seats directly; SC/ST women versus SC/ST-open seats by subtracting the two coefficients against fully open seats. The latter SE cannot be recovered exactly without covariance. Its attainable range is `abs(se1-se0)` to `se1+se0`, and the paper displays a conservative normal-approximation envelope using the upper endpoint. This recovers subgroup schooling differences, not an overall caste-adjusted coefficient. That coefficient additionally requires the covariate design/weights (or the microdata); a standardized version requires the relevant schooling variance. No missing covariance, weights or SDs have been invented.

The “Mincer residual” is a household-asset residual after individual and household controls and village effects, estimated separately by identity group. Relative educational selection uses a politician's position against their own group's population distribution. Neither is the attained-schooling contrast or a direct observation of governing performance. We retain the schooling comparisons without adopting those interpretations of quality.
