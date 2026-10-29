
<!-- README.md is generated from README.Rmd. Please edit that file -->

# SEQIC Report 2026 <img src="man/figures/README/HHS PFP 4.png" align="right" width="200" alt="" />

<!-- badges: start -->

[![Open
Source](https://img.shields.io/badge/license-MIT-green.svg)](LICENSE)
<!-- badges: end -->

The SEQIC Report 2026 project is a reproducible, open-source analytic
workflow designed to generate standardized trauma quality indicators for
Iowa-verified trauma centers. It covers data from calendar years
2021–2025, with 2025 representing the most current year of analysis.
This report documents state-level trauma system performance in Iowa.
Trauma centers all receive individualized reports on the SEQIC and
risk-adjusted benchmarking, but that is not within the scope of this
report. The written report related to the analyses documented in this
repository can be found on trauma registry webpage of the [Bureau of
Emergency Medical and Trauma
Services](https://hhs.iowa.gov/health-prevention/providers-professionals/emergency-medical-services-trauma/trauma-system/trauma-data-registry)
of Iowa HHS.

This project operationalizes the trauma performance metrics developed by
the System Evaluation and Quality Improvement Committee (SEQIC)—a
multi-disciplinary group under the Iowa Department of Health and Human
Services (HHS). The indicators reflect key aspects of trauma system
performance, including timeliness, triage accuracy, data completeness,
and risk adjusted mortality calculations.

A change to the 2026 report as compared to previous state reporting
years is that Indicators 4a and 4b will not be included. However, Iowa
trauma centers can continue to expect to see statistics on these
indicators coming to them as per usual.

## {traumar}

This report utilizes the `traumar` R package (CRAN,
[doi:10.32614/CRAN.package.traumar](https://doi.org/10.32614/CRAN.package.traumar))
to generate all statistical outputs related to trauma system
performance. The `traumar` package provides validated functions grounded
in peer-reviewed literature to support Continuous Quality Improvement
(CQI) and Process Improvement (PI) in trauma care. Metrics calculated in
this report—including the W-Score, M-Score, Z-Score, Relative Mortality
Metric (RMM), triage accuracy indicators, and other SEQIC indicators—are
derived using methods embedded in `traumar`. These methods are supported
by publications such as Napoli et al. (2017), Boyd et al. (1987),
Champion et al. (1989), and others. For detailed information on the
methodology, refer to the package documentation at
<https://bemts-hhs.github.io/traumar>.

## Summary of the SEQIC Indicators 2026

| Indicator | Title | Definition |
|:--:|:--:|:--:|
| 1a | Trauma surgeon in ED \<= 15 min (Level I/II) | Proportion of Level I activations with surgeon arrival \<= 15 min. Requires calculable time from ED arrival to trauma team arrival. |
| 1b | Trauma surgeon in ED \<= 30 min (Level III) | As 1a, but applies to Level III facilities and uses a 30-minute threshold. |
| 1c | Trauma surgeon response time unknown | Proportion of Level I activations where trauma surgeon response time is uncalculable. Denominator includes all ‘Surgery/Trauma’ team members per incident. |
| 1d | 1st physician in ED \<= 5 min (Level I/II) | Proportion of Level I/II activations with a physician (defined set) arriving \<= 5 min. from ED arrival. |
| 1e | 1st physician in ED \<= 20 min (Level III/IV) | As 1d, but applies to Level III/IV facilities with a 20-minute threshold. |
| 1f | Physician response time unknown | Proportion of Level I/II activations where physician response time is uncalculable. Includes all defined service types per incident. |
| 2 | Missing injury time | Proportion of incidents missing injury time field. |
| 3 | Probability of Survival score present | Proportion of incidents with a valid Probability of Survival score (requires ISS, RTS, age, trauma type). |
| 5a | Blood ETOH measured | Proportion of all incidents where blood ETOH was measured. Includes pediatric cases. |
| 5b | Blood ETOH positive | Proportion of blood ETOH tests that were positive. |
| 5c | Drug screen completed | Proportion of all incidents with a drug screen completed. Includes pediatric cases. |
| 5d | Drug screen positive | Proportion of completed drug screens that were positive. |
| 6 | GCS \< 9, definitive care arrival \>3 hrs (transferred) | Proportion of transfers with GCS \< 9 arriving at definitive care \>3 hrs from injury. Includes only calculable cases with defined transfer. |
| 7 | Arrival at definitive care \>3 hrs from injury | Proportion of all patients arriving at definitive care \>3 hrs after injury time. |
| 8 | Survival rate by risk category and trauma level | Survival rate stratified by trauma hospital level and patient risk: High, Moderate, Low (based on Ps, ISS, GCS, vitals). Missingness affects stratification. |
| 9a | ED LOS \> 2 hrs in acute transfers | Proportion of acute transfers with ED LOS \>2 hrs. Requires complete timestamps and valid transfer status. |
| 9b | ED LOS \> 3 hrs in acute transfers | Same as 9a but with threshold \>3 hrs. |
| 9c | ED decision time \> 60 min in acute transfers | Acute transfers with \>60 min from arrival to decision to discharge. Requires complete decision timestamps. |
| 9d | ED decision time \> 120 min in acute transfers | Same as 9c but with threshold \>120 min. |
| 9e | ED discharge delay \> 60 min post-decision | Acute transfers with \>60 min from decision to physical discharge. Requires both timestamps present. |
| 9f | ED discharge delay \> 120 min post-decision | Same as 9e but with threshold \>120 min. |
| 10a | Under-triage (Cribari method) | Proportion of ISS \>15 or NFTI+ not receiving full trauma team activation. Denominator includes all non-fully activated, non-transferred incidents. |
| 10b | Over-triage | Proportion of fully activated cases (Level 1) with ISS \<= 15 among kept patients. |
| 10c | Under-triage (Modified Cribari) | Same numerator as 10a. Denominator includes all ISS \>15 or NFTI+ cases (more conservative). |
| 11 | Low ISS, \<24h ED stay at definitive care (transfers) | Transferred-in patients with ISS \<9 discharged from ED in \<24 hrs. Denominator is all definitive care transfers. |
| 12 | Incident submitted within 60 days of discharge | Proportion of incidents submitted within 60 days of discharge. 80% threshold per data dictionary. |
| 13 | Incident has validity score \>= 85% | Proportion of incidents with registry validity score \>= 85%. |
