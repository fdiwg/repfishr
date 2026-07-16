# Fills an ICCAT template workbook with dataset characteristics information

Fills an ICCAT template workbook with dataset characteristics
information

## Usage

``` r
iccat_fill_report_dataset_characteristics(
 wb, reporting_flag, from, to,
 report_version, report_type, report_coverage
)
```

## Arguments

- wb:

  a Workbook object from openxlsx

- reporting_flag:

  reporting flag

- from:

  start year/date

- to:

  end year/date

- report_version:

  report version (`Final`, `Preliminary`)

- report_type:

  report type

- report_coverage:

  report coverage

- notes:

  notes

## Value

the modified Workbook object
