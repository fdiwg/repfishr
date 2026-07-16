# reporting_flow

reporting_flow

reporting_flow

## Format

[`R6Class`](https://r6.r-lib.org/reference/R6Class.html)

## Value

Object of class [`R6Class`](https://r6.r-lib.org/reference/R6Class.html)
for modelling a reporting flow

## Author

Emmanuel Blondel \<emmanuel.blondel1@gmail.com\>

## Public fields

- `sender`:

  sender object of class
  [reporting_sender](https://fdiwg.github.io/repfishr/reference/reporting_sender.md)

## Methods

### Public methods

- [`reporting_flow$new()`](#method-reporting_flow-new)

- [`reporting_flow$getSender()`](#method-reporting_flow-getSender)

- [`reporting_flow$getReceivers()`](#method-reporting_flow-getReceivers)

- [`reporting_flow$getReceiverIds()`](#method-reporting_flow-getReceiverIds)

- [`reporting_flow$getReceiver()`](#method-reporting_flow-getReceiver)

- [`reporting_flow$clone()`](#method-reporting_flow-clone)

------------------------------------------------------------------------

### Method `new()`

Initializes a reporting flow

#### Usage

    reporting_flow$new(sender, sender_type)

#### Arguments

- `sender`:

  sender id

- `sender_type`:

  type of sender

------------------------------------------------------------------------

### Method `getSender()`

Get sender

#### Usage

    reporting_flow$getSender()

#### Returns

an object of class
[reporting_sender](https://fdiwg.github.io/repfishr/reference/reporting_sender.md)

------------------------------------------------------------------------

### Method `getReceivers()`

Get list of valid receivers for the selected sender

#### Usage

    reporting_flow$getReceivers(raw = FALSE)

#### Arguments

- `raw`:

  raw

#### Returns

an object of class [data.frame](https://rdrr.io/r/base/data.frame.html)
or a `list` of
[reporting_receiver](https://fdiwg.github.io/repfishr/reference/reporting_receiver.md)

------------------------------------------------------------------------

### Method `getReceiverIds()`

Get list of valid receivers IDs

#### Usage

    reporting_flow$getReceiverIds()

------------------------------------------------------------------------

### Method `getReceiver()`

Get a receiver by its id

#### Usage

    reporting_flow$getReceiver(id)

#### Arguments

- `id`:

  id

#### Returns

an object of class
[reporting_receiver](https://fdiwg.github.io/repfishr/reference/reporting_receiver.md)

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    reporting_flow$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
