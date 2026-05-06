# C2PROCTTEST
An R package that replicates the output of **SAS PROC TTEST** — tables and plots — for all three t-test types.

---

## What it does
| Function | Test |
|---|---|
| `sas_one_sample()` | One-sample t-test |
| `sas_two_sample()` | Two independent samples t-test |
| `sas_paired()` | Paired samples t-test |

Each function produces:
- **Console tables** matching SAS PROC TTEST formatting (Statistics, Confidence Limits, T-Tests, Equality of Variances)
- **Diagnostic plots** in the R graphics window matching SAS default ODS plots

---

## Installation
```r
devtools::install_github("DrSubbiah/C2PROCTTEST")
```

---

## Sample Datasets
Two sample CSV datasets are available directly in this repository for testing and demonstration:

| File | Description | Used in |
|---|---|---|
| `bweight.csv` | Birth weight data with `bweight` and `sex` columns | `sas_one_sample()`, `sas_two_sample()` |
| `sleep.csv` | Sleep extra hours data with `extra` and `group` columns | `sas_paired()` |

Download them from the repo and load with:
```r
bw    <- read.csv("bweight.csv")
sleep <- read.csv("sleep.csv")
```

---

## Usage
```r
library(C2PROCTTEST)

# One-sample t-test
sas_one_sample(bw$bweight, mu = 0, var_name = "birth weight")

# Two independent samples t-test
sas_two_sample(bw$bweight, bw$sex, var_name = "birth weight")

# Paired samples t-test
x <- sleep$extra[sleep$group == 1]
y <- sleep$extra[sleep$group == 2]
sas_paired(x, y, var_name = "group1 - group2")
```

---

## Output matches SAS
| Feature | Included |
|---|---|
| N, Mean, Std Dev, Std Err, Min, Max per group | Yes |
| 95% CI for Mean | Yes |
| 95% CI for Std Dev (chi-square equal-tailed) | Yes |
| Both Pooled and Satterthwaite rows | Yes |
| Folded F test (Equality of Variances) | Yes |
| Num DF / Den DF (larger variance in numerator) | Yes |
| SAS decimal rounding (1 dp mean/SD, 4 dp SE, 2 dp t/F) | Yes |
| Diagnostic plots | Yes |

---

## Dependencies
Base R only (`stats`, `graphics`, `grDevices`). No external packages required.
