
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ergoMM: Multimorbidity in Rotterdam Study

This is an internal package of the Rotterdam Study to make working with
follow-up data for multiple diseases easier. At the moment, it only
serves to redefines prevalence of (so far) eleven diseases with respect
to user defined baseline date.

So far, the package works for RS-I, RS-II and RS-III cohorts and the
following diseases:

- Cancer (all types expect non-melanoma skin cancer)
- COPD
- Asthma
- Stroke
- Diabetes
- Coronary heart disease
- Heart failure
- Transient Ischemic Attack
- Dementia
- Parkinsonism
- Depression (including depressive symptoms)
- Chronic kidney disease

## Installation

You can install ergoMM from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("frenkxs/ergoMM")
```

## Example

The function `get_prev` takes user specified baseline dates for each
participant (or a subset of participants) and redefines prevalence for a
set of diseases. As a bonus, the number of prevalent diseases at
baseline for each participant is also returned.

It takes the following arguments:

**fu_startd** data frame with exactly two columns: `ergoid` and
`fu_startd`. `ergoid` contains the ids of participants for which the
prevalence should be redefined. It can also be a subset of participants
from RS-I through RS-III. `fu_startd` contains the baseline dates
corresponding to each participant. Participant with baseline dates set
to NA will be removed. `ergoid` should be formatted as factor,
`fu_startd` is a date in the yyyy-mm-dd format.

If the baseline dates are the same for all participants in the data,
then you may use a sigle date value in the yyyy-mm-dd format.

**data** ERGO multimorbidity data. The default is shift_data. To access
the ERGO multimorbidity data, contact Frank van Rooij (f.vanrooij at
erasmusmc.nl) and Silvan Licher (s.licher at erasmusmc.nl).

**diseases** a character vector with diseases for which the baseline
prevalence should be redefined. By default all diseases included in the
multimorbidity data will be considered. A subset of the diseases can be
defined using a the following abbreviations:

- dia: diabetes
- stroke: stroke
- can1: cancer
- dem: dementia
- hf: heart failure
- chd: coronary heart disease
- park: parkinsonism
- dep1: depression
- COPD: chronic obstructive pulmonary disease
- asthma: asthma
- tia: transient ischemic attack
- ckd: chronic kidney disease

**removeNA** whether participants with incomplete follow-up should be
removed. The default is set to FALSE, meaning data for all participants
specified in fu_startd will be returned. The missing data are coded,
using numbers from 100 onwards; the reason for missing data are
explained below (and printed on the console when calling the function):

- 101: missing follow up start date
- 102: missing follow up end date
- 103: missing prevalence indicator
- 104: missing incidence indicator
- 105: your baseline date is earlier than ERGO follow up start
- 106: your baseline date is later than censor date (for disease free
  individuals)
- 107: any other reason not covered above (should not happen)

``` r
library(ergoMM)
# install.packages('lubridate")

# load the multimorbidity dataset (an example)
load(here::here("data", "shift_data.RData"))

# generate fu_startd dataset with new baseline date
baseline <- data.frame(ergoid = shift_data$ergoid, 
                       fu_startd = shift_data$fp_startdate + lubridate::dyears(rnorm(nrow(shift_data), 5)))

# redefines prevalence for all diseases and keeps participants with missing data in
cohort01 <- get_prev(fu_startd = baseline, 
                     data = shift_data)

# redefines prevalence for all diseases and removes participants with missing data
cohort02 <- get_prev(fu_startd = baseline, 
                     data = shift_data, 
                     removeNA = TRUE)

# redefines prevalence for only diabetes and cancer and removes participants with missing data
cohort03 <- get_prev(fu_startd = baseline, 
                   data = shift_data, 
                   diseases = c("dia", "can1"),
                   removeNA = TRUE)
```

## Contact

Contact Premysl Velek (p.velek at erasmusmc.nl) and Silvan Licher
(s.licher at erasmusmc.nl) for any questions about the package.
