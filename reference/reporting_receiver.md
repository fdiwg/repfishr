# reporting_receiver

reporting_receiver

reporting_receiver

## Format

[`R6Class`](https://r6.r-lib.org/reference/R6Class.html)

## Value

Object of class [`R6Class`](https://r6.r-lib.org/reference/R6Class.html)
for modelling a reporting receiver

## Author

Emmanuel Blondel \<emmanuel.blondel1@gmail.com\>

## Super class

[`repfishr::reporting_actor`](https://fdiwg.github.io/repfishr/reference/reporting_actor.md)
-\> `reporting_receiver`

## Public fields

- `sender`:

  sender

- `tasks`:

  tasks

## Methods

### Public methods

- [`reporting_receiver$new()`](#method-reporting_receiver-new)

- [`reporting_receiver$setSender()`](#method-reporting_receiver-setSender)

- [`reporting_receiver$getTasks()`](#method-reporting_receiver-getTasks)

- [`reporting_receiver$getTaskDefinitions()`](#method-reporting_receiver-getTaskDefinitions)

- [`reporting_receiver$getTaskDefinitionById()`](#method-reporting_receiver-getTaskDefinitionById)

- [`reporting_receiver$clone()`](#method-reporting_receiver-clone)

------------------------------------------------------------------------

### Method `new()`

Initializes a reporting receiver

#### Usage

    reporting_receiver$new(id, name = NULL, type = NULL)

#### Arguments

- `id`:

  id

- `name`:

  name

- `type`:

  type

------------------------------------------------------------------------

### Method `setSender()`

Set sender

#### Usage

    reporting_receiver$setSender(sender)

#### Arguments

- `sender`:

  sender object of class
  [reporting_sender](https://fdiwg.github.io/repfishr/reference/reporting_sender.md)

------------------------------------------------------------------------

### Method `getTasks()`

Get task IDs

#### Usage

    reporting_receiver$getTasks()

#### Returns

the list of task IDs

------------------------------------------------------------------------

### Method `getTaskDefinitions()`

Get tasks

#### Usage

    reporting_receiver$getTaskDefinitions(raw = FALSE)

#### Arguments

- `raw`:

  raw

#### Returns

a list of
[reporting_task](https://fdiwg.github.io/repfishr/reference/reporting_task.md)

------------------------------------------------------------------------

### Method `getTaskDefinitionById()`

Get task definition by ID

#### Usage

    reporting_receiver$getTaskDefinitionById(id)

#### Arguments

- `id`:

  id

#### Returns

an object of class
[reporting_task](https://fdiwg.github.io/repfishr/reference/reporting_task.md)

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    reporting_receiver$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
