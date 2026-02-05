# camr_process_redcap

Generate targets pipelines and tidy datasets from REDCap projects, including
subject-level, subject-visit, and repeating-instrument outputs, plus an
automated Quarto report. Subject-level data are built from forms that appear
in only one event.

## Quick start

```r
library(camr_process_redcap)

write_redcap_targets(
  redcap_url = "https://redcap.example.edu/api/",
  api_token = Sys.getenv("REDCAP_API_TOKEN"),
  out_dir = "camr_redcap_pipeline"
)
```

Set your token before running the pipeline:

```r
Sys.setenv(REDCAP_API_TOKEN = "<your_token>")
```

Then run `targets::tar_make()` in the directory containing `_targets.R`.
`out_dir` must point to a new folder; the function will create it and
initialize a git repository with an initial commit.

The pipeline validates `record_status` values against REDCap metadata and
warns if the configured values do not match the available choices.
