# reporting_format

reporting_format

reporting_format

## Format

[`R6Class`](https://r6.r-lib.org/reference/R6Class.html)

## Value

Object of class [`R6Class`](https://r6.r-lib.org/reference/R6Class.html)
for modelling a reporting format

## Author

Emmanuel Blondel \<emmanuel.blondel1@gmail.com\>

## Public fields

- `id`:

  id

- `name`:

  name

- `ref`:

  ref format specification link

- `spec`:

  format specification object of class
  [format_spec](https://fdiwg.github.io/vrule/reference/format_spec.html)

## Methods

### Public methods

- [`reporting_format$new()`](#method-reporting_format-new)

- [`reporting_format$clone()`](#method-reporting_format-clone)

------------------------------------------------------------------------

### Method `new()`

Initializes a reporting task

#### Usage

    reporting_format$new(id, name, ref)

#### Arguments

- `id`:

  id

- `name`:

  name

- `ref`:

  ref

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    reporting_format$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
