# CHANGELOG

## v0.3.0

### Bug Fixes

The default `Harnais.Error.export_exception/2` clause was
returning an incorrectly formatted export keyword.

### New Functions

`Harnais.Error.gather_export/1` takes an export, usually from
`Harnais.Error.export/2` or `Harnais.Error.export_exception/2`, and
tries to gathers all the values for the same key (e.g. `:ok` and `:error`) into a list.
Usually the `:error` list will be a list of `Keyword`.

## v0.2.0

### Bug Fixes

Fixed API Reference link in the README.

### Changed Functions

The export functions for both `Harnais.Error` and
`Harnais.Error.Status` now take an *opts* as the optional second
argument. The *opts*, if any, are used to update (`update/2`) the exception before
performing the export.

The keys in the `:export_config` used by `Harnais.Error`'s default export function
are now normalised so that aliases can be used e.g. `:m` for
`:message`, and also validated: unknown keys cause an error.

### New Functions

`Harnais.Error.export_exception/2` takes an `Exception` and optional
*opts* and creates an export, returning `{:ok, export}` or `{:error, error}`.

## v0.1.0

The Exception for the Harnais Package Family.


