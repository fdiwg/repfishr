# reporting_task

reporting_task

reporting_task

## Format

[`R6Class`](https://r6.r-lib.org/reference/R6Class.html)

## Value

Object of class [`R6Class`](https://r6.r-lib.org/reference/R6Class.html)
for modelling a reporting task

## Author

Emmanuel Blondel \<emmanuel.blondel1@gmail.com\>

## Public fields

- `sender`:

  sender

- `receiver`:

  receiver

- `id`:

  id

- `name`:

  name

- `context`:

  context

- `measurement`:

  measurement

- `formats`:

  formats

- `process_fun`:

  process handler (function)

- `report_fun`:

  report handler (function)

- `report_data`:

  data to report

- `report_metadata`:

  metadata to report

## Methods

### Public methods

- [`reporting_task$new()`](#method-reporting_task-new)

- [`reporting_task$setSender()`](#method-reporting_task-setSender)

- [`reporting_task$process()`](#method-reporting_task-process)

- [`reporting_task$report()`](#method-reporting_task-report)

- [`reporting_task$clone()`](#method-reporting_task-clone)

------------------------------------------------------------------------

### Method `new()`

Initializes a reporting task

#### Usage

    reporting_task$new(receiver = NULL, file = NULL, task = NULL)

#### Arguments

- `receiver`:

  receiver

- `file`:

  file

- `task`:

  task (as list object)

------------------------------------------------------------------------

### Method `setSender()`

Set sender

#### Usage

    reporting_task$setSender(sender)

#### Arguments

- `sender`:

  sender object of class
  [reporting_sender](https://fdiwg.github.io/repfishr/reference/reporting_sender.md)

------------------------------------------------------------------------

### Method `process()`

Process data before reporting

#### Usage

    reporting_task$process(data, metadata, path, parallel = FALSE, ...)

#### Arguments

- `data`:

  object of class [data.frame](https://rdrr.io/r/base/data.frame.html)

- `metadata`:

  metadata object

- `path`:

  path for the output file

- `parallel`:

  whether data validation should be run in parallel

- `...`:

  any other arguments to be passed to vrule validation method

------------------------------------------------------------------------

### Method `report()`

Reports data

#### Usage

    reporting_task$report(data, metadata, path, parallel = FALSE, ...)

#### Arguments

- `data`:

  object of class [data.frame](https://rdrr.io/r/base/data.frame.html)

- `metadata`:

  metadata object

- `path`:

  path for the output file

- `parallel`:

  whether data validation should be run in parallel

- `...`:

  any other arguments to be passed to vrule validation method

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    reporting_task$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
