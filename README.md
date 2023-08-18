# rpapersurvey

[![R-CMD-check](https://github.com/agency-fund/rpapersurvey/workflows/R-CMD-check/badge.svg)](https://github.com/agency-fund/rpapersurvey/actions)
[![codecov](https://codecov.io/gh/agency-fund/rpapersurvey/branch/main/graph/badge.svg)](https://codecov.io/gh/agency-fund/rpapersurvey)

## Overview

[PaperSurvey.io](https://www.papersurvey.io) is a platform for generating printable paper surveys and then converting the completed surveys to digital data using image recognition. The `rpapersurvey` R package uses the [PaperSurvey.io API](https://www.papersurvey.io/app/settings/developer) to interact programmatically with the service.

API access is currently only available to customers on an Enterprise Plus plan.

## Installation

```r
if (!requireNamespace('remotes', quietly = TRUE))
  install.packages('remotes')
remotes::install_github('agency-fund/rpapersurvey')
```

## Usage

A basic example:

```r
library('data.table')
library('rpapersurvey')

psio_set_api_key('PATH_TO_API_KEY')

# fetch metadata for all surveys
surveys = psio_get_surveys()

# fetch entries for a given survey
entries = psio_get_entries(surveys$id[1L])

# extract answers from entries
answers = psio_get_answers(entries)
```

For more details, see the [reference documentation](https://agency-fund.github.io/rpapersurvey/reference/index.html).
