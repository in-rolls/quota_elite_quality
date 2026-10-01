# Councillor qualifications in the Karekurve-Ramachandra and Lee deposits

[The deposit comparison](../R/urban.R), run by `make check`, compares two replication deposits with the winners' MyNeta affidavit summaries. It returns the comparison results in memory for the manuscript and tests.

| Deposit | Field | Winners compared | Agreement with MyNeta | Agreement if values were shuffled |
|---|---|---:|---:|---:|
| AJPS, Delhi 2012 (doi:10.7910/DVN/0QOVCH) | Graduate or not | 160 | 53.8% | 52.8% |
| AJPS, Delhi 2012 | Education level (rank correlation) | 161 | −0.01 | |
| AJPS, Delhi 2012 | Declared assets, exact | 208 | 97.1% | |
| CPS, Mumbai 2012 council (doi:10.7910/DVN/IO9SLQ) | Graduate or not | 163 | 95.7% | 58.1% |
| CPS, Mumbai 2012 council | Pending cases, exact | 166 | 94.6% | |

The Delhi 2012 education values carry no information about the winners they are attached to: agreement is what random assignment would give. The same rows' declared assets match MyNeta almost exactly, so the rows refer to the right people and only the education field is affected. The deposit's five-level `edu_recoded` is a direct recode of the education labels in the `local_elections` 2012 file, which has the same problem. For the Delhi 2017 winners, the same labels agree with MyNeta for 261 of 262 jointly classified winners (computed by `R/urban.R`). The Mumbai deposit agrees with MyNeta on both fields.

The deposit's AJPS do-file uses `edu_recoded` for 2012 and 2017 winners in Table A.12. The 2012 values are not a row offset of the right ones: shifting the legacy file by up to 40 rows in either direction never matches more than 19% of winners' MyNeta education labels exactly, against 11% with no shift (computed by `R/urban.R`). Where they came from remains unknown. MyNeta summarizes self-declared affidavits; the scans have not been re-read.

How the join works: the deposit's 2012 rows have no ward number, so winners are matched to the `local_elections` 2012 file on vote count and party (pairs that repeat are dropped), and to MyNeta's 2012 winners table by ward. The Mumbai deposit is matched to MyNeta's BMC 2012 winners by ward, using one survey wave per councillor from 2013 to 2016.
