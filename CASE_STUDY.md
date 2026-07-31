# Case Study — Campaign Impact Analyzer

**Live app:** https://aykhaled-campaign-impact-analyzer.share.connect.posit.cloud/
**Code:** https://github.com/aykhaled/campaign-impact-analyzer

---

## The problem

Most campaign analysis produces a number and a p-value. Neither tells you whether the number can be trusted.

The failure mode is specific and expensive: a comparison of treated and untreated groups looks decisive, gets presented with a tight confidence interval, and is simply wrong — because the two groups differed before the campaign ever ran. Nothing in the output signals a problem. The analysis is confidently incorrect, and budget gets allocated against it.

I built a tool that measures exactly how large that error can be, and warns when the conditions for it are present.

## What it does

The tool takes campaign or before/after data and works through three questions in order.

**Which method does this data support?** Randomised assignment, difference-in-differences, and propensity adjustment rest on different assumptions of very different strength. The tool determines which are available, states which are ruled out and why, and recommends one — including saying when a computable method would be the wrong choice.

**What is the effect, and could this design have detected it?** Every estimate is reported alongside the minimum effect the design could reliably identify. This distinguishes "the campaign didn't work" from "this test couldn't have told you either way" — a distinction that routinely goes unstated. In the sample data it surfaced that one campaign's revenue effect sat close to the detection floor, so its magnitude was poorly pinned down even though it was statistically significant.

**Which assumptions actually hold?** Covariate balance, group sizes, parallel trends, and common support are each checked and reported in plain language, pass or fail.

## The test that proves it works

Claims about statistical methods are usually unfalsifiable in practice, because the true effect is unknown. Here it isn't.

Starting from a genuine randomised experiment — 64,000 customers, three arms — the true campaign effect is known. I then deliberately broke the randomisation to create a confounded sample, ran each estimator on it, and scored the results against the truth.

**The unadjusted comparison overstated the campaign's impact by 46%, with a confidence interval that excluded the true value entirely. The adjusted estimators recovered it to within 6–12%.**

The tool also flagged poor covariate overlap on that sample — warning that the adjustment was extrapolating beyond what the data supported — even though the estimate happened to land close. A tool built to demonstrate well would have suppressed that warning. This one reports it, because on a client's real data the same warning is the difference between a defensible finding and an expensive mistake.

## Repeatability

Column mappings, outcome definitions, control group, significance level, and validity thresholds all live in a configuration file. **Pointing the tool at a different company's data is a configuration change, not a rebuild.**

The statistical engine is a set of pure functions with unit tests that assert its estimates reproduce a published benchmark. The app and the generated report call the same functions, so they cannot disagree about a number.

**Built with:** R, Shiny, Quarto, `renv`, deployed to Posit Connect Cloud with automatic republishing on push.
