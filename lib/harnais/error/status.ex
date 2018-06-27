defmodule Harnais.Error.Status do
  @moduledoc ~S"""
  `Harnais.Error.Status` manages an ordered collection of `{:ok, value}` and
  `{:error, error}` results.

  `Harnais.Error.Status` is an `Exception` and supports the
  usual `exception/1` and `message/1` callbacks.

  The module supports overriding the default `message/1` function -
  see `message/1` and the `:message_function` field.

  The module supports also exporting its results (see `export/1`). The
  default exporter can be overriden - see the `:export_function` field.

  See `Harnais.Error` for an overview and explanation of other documentation terms.

  ## Documentation Terms

  In the documentation these terms, usually in *italics*, are used to mean the same thing.

  ### *status*

  An instance of the module's `struct`.

  ### *error* and *errors*

  An instance of an `Exception` `struct`, often `Harnais.Error`.

  A *errors* is a list of *error*

  ### *ok* and *oks*

  An *ok* is any valid term. An *oks* is a list of *ok*.

  ### *result* and *results*

  A *result* is either `{:ok, value}` or `{:error, error}` where `error` is an `Exception`.

  A *results* is a list of *result*.

  ## Module State

  The module holds it state is a `struct` with the following fields:

  | Field | Purpose |
  | :---       | :---            |
  | `:entries` | *the list of results* |
  | `:message_function` | *See Harnais.Error*  |
  | `:message_config` | *See Harnais.Error* |
  | `:export_function` | *See Harnais.Error* |
  | `:export_config` | *See Harnais.Error* |

  """

  use Plymio.Codi
  require Plymio.Fontais.Option
  alias Harnais.Error, as: HEE
  use Harnais.Attribute
  use Harnais.Error.Attribute

  @codi_opts [
    {@plymio_codi_key_vekil, Plymio.Vekil.Codi.__vekil__()}
  ]

  import Plymio.Fontais.Guard,
    only: [
      is_value_unset: 1,
      is_value_unset_or_nil: 1,
      is_filled_list: 1,
      is_empty_list: 1
    ]

  import Plymio.Fontais.Utility,
    only: [
      list_wrap_flat_just: 1
    ]

  import Plymio.Fontais.Option,
    only: [
      opts_create_aliases_dict: 1,
      opts_canonical_keys: 2
    ]

  import Plymio.Fontais.Result,
    only: [
      validate_results: 1
    ]

  import Plymio.Funcio.Enum.Map.Collate,
    only: [
      map_collate0_enum: 2
    ]

  import Plymio.Fontais.Error.Format,
    only: [
      format_error_message_value: 1
    ]

  defp new_error_result_no_results() do
    HEE.new_error_result(m: "Harnais.Error.Status has no results")
  end

  defp new_error_result_no_oks() do
    HEE.new_error_result(m: "Harnais.Error.Status has no oks")
  end

  defp new_error_result_no_errors() do
    HEE.new_error_result(m: "Harnais.Error.Status has no errors")
  end

  defp error_new_error_result(opts) do
    HEE.new_error_result(opts)
  end

  @doc false

  defguard is_empty_answer(value) when is_value_unset_or_nil(value) or is_empty_list(value)

  @harnais_error_status_kvs_aliases [
    {@harnais_error_field_entries, nil},
    @harnais_error_field_alias_message_function,
    @harnais_error_field_alias_message_config,
    @harnais_error_field_alias_export_function,
    @harnais_error_field_alias_export_config,

    # virtual
    {@harnais_error_status_verb_add_ok, nil},
    {@harnais_error_status_verb_add_error, nil},
    {@harnais_error_status_verb_add_result, nil},
    {@harnais_error_status_verb_add_oks, nil},
    {@harnais_error_status_verb_add_errors, nil},
    {@harnais_error_status_verb_add_results, nil}
  ]

  @harnais_error_status_dict_aliases @harnais_error_status_kvs_aliases
                                     |> opts_create_aliases_dict

  @doc false

  def update_canonical_opts(opts, dict \\ @harnais_error_status_dict_aliases) do
    opts |> opts_canonical_keys(dict)
  end

  @harnais_error_status_defexception [
    {@harnais_error_field_entries, @harnais_error_status_value_entries_initial},
    {@harnais_error_field_message_function, @plymio_fontais_the_unset_value},
    {@harnais_error_field_message_config, @plymio_fontais_the_unset_value},
    {@harnais_error_field_export_function, @plymio_fontais_the_unset_value},
    {@harnais_error_field_export_config, @plymio_fontais_the_unset_value}
  ]

  defexception @harnais_error_status_defexception

  @type ok :: any
  @type oks :: [ok]

  @type errors :: [error]

  @type results :: [result]

  @doc_new ~S"""

  `new/1` creates a new instance of the status exception, returning `{:ok, status}`.

  `new/1` takes optional (`Keyword`) options (`opts`) where the keys can be one of the `add_` functions (e.g. `add_ok/2`).

  ## Examples

      iex> {:ok, status} = new()
      ...> match?(%Harnais.Error.Status{}, status)
      true

      iex> {:ok, t} = new(
      ...>   add_result: {:ok, 42},
      ...>   add_error: :failed_miserably,
      ...>   add_ok: :worked_fine,
      ...>   add_errors: [:e1, :e2])
      ...> t |> export!
      [ok: 42, error: [[v: :failed_miserably]], ok: :worked_fine, error: [[v: :e1]], error: [[v: :e2]]]

  """

  [
    # update proxies
    {@plymio_codi_pattern_proxy_put,
     [
       state_def_new_doc: quote(do: @doc(unquote(@doc_new))),
       state_def_update_doc:
         quote do
           @doc ~S"""
           `update/2` takes an `instance` of the module's `struct` and an optional *opts*.

           The *opts* are normalised by calling the module's `update_canonical_opts/1`
           and then reduced with `update_field/2`:

               opts |> Enum.reduce(instance, fn {k,v}, s -> s |> update_field({k,v}) end)

           `{:ok, instance}` is returned.
           """
         end,
       state_def_new_since: quote(do: @since("0.1.0")),
       state_def_new_since!: quote(do: @since("0.1.0")),
       state_def_update_since: quote(do: @since("0.1.0")),
       state_def_update_since!: quote(do: @since("0.1.0"))
     ]},
    {@plymio_codi_pattern_proxy_fetch,
     [
       :state_base_package,
       :state_defp_update_field_header
     ]},
    {@plymio_codi_pattern_proxy_fetch,
     [
       {@plymio_codi_key_proxy_name, :state_defp_update_proxy_field_normalise},
       {@plymio_fontais_key_forms_edit,
        [
          {@plymio_fontais_key_rename_atoms, [proxy_field: @harnais_error_field_entries]},
          {@plymio_fontais_key_rename_funs, [proxy_field_normalise: :validate_results]}
        ]}
     ]},
    {@plymio_codi_pattern_proxy_fetch,
     [
       {@plymio_codi_key_proxy_name, :state_defp_update_proxy_field_fun1},
       {@plymio_fontais_key_forms_edit,
        [
          {@plymio_fontais_key_rename_atoms,
           [proxy_field: @harnais_error_field_message_function]},
          {@plymio_fontais_key_rename_funs, [new_error_result: :error_new_error_result]}
        ]}
     ]},
    {@plymio_codi_pattern_proxy_fetch,
     [
       {@plymio_codi_key_proxy_name, :state_defp_update_proxy_field_fun1},
       {@plymio_fontais_key_forms_edit,
        [
          {@plymio_fontais_key_rename_atoms, [proxy_field: @harnais_error_field_export_function]},
          {@plymio_fontais_key_rename_funs, [new_error_result: :error_new_error_result]}
        ]}
     ]}
  ]
  |> CODI.reify_codi(@codi_opts)

  defp update_field(%__MODULE__{} = status, {k, v})
       when k in @harnais_error_status_verbs do
    apply(__MODULE__, k, [status, v])
  end

  [
    {@plymio_codi_pattern_proxy_fetch,
     [
       {@plymio_codi_key_proxy_name, :state_defp_update_field_unknown},
       {@plymio_fontais_key_forms_edit,
        [
          {@plymio_fontais_key_rename_funs, [new_error_result: :error_new_error_result]}
        ]}
     ]}
  ]
  |> CODI.reify_codi(@codi_opts)

  defp update_entries(status, entries) do
    status |> update_field({@harnais_error_field_entries, entries})
  end

  @doc ~S"""
  `size/1` returns the number of results in the *status*.

  ## Examples

      iex> new!() |> size
      0

      iex> new!() |> add_error!(:error1) |> add_ok!(:ok1) |> size
      2

  """

  @since "0.1.0"

  @spec size(t) :: integer

  def size(t)

  def size(%__MODULE__{
        @harnais_error_field_entries => @harnais_error_status_value_entries_initial
      }) do
    0
  end

  def size(%__MODULE__{@harnais_error_field_entries => entries}) do
    entries |> length
  end

  @doc ~S"""
  `empty?/1` returns `true` if the *status* has no *results*, else `false`.

  ## Examples

      iex> new!() |> empty?
      true

      iex> new!() |> add_error!(:error1) |> add_ok!(:ok1) |> empty?
      false

  """

  @since "0.1.0"

  @spec empty?(t) :: true | false

  def empty?(t)

  def empty?(%__MODULE__{
        @harnais_error_field_entries => @harnais_error_status_value_entries_initial
      }) do
    true
  end

  def empty?(%__MODULE__{@harnais_error_field_entries => entries}) do
    case entries |> length do
      0 -> true
      _ -> false
    end
  end

  @doc ~S"""
  `reset/1` reinitialised the *status*.

  ## Examples

      iex> {:ok, status} = new!() |> reset()
      ...> status |> empty?
      true

      iex> {:ok, status} = new!() |> add_error!(:error1) |> add_ok!(:ok1) |> reset()
      ...> status |> size
      0

  """

  @since "0.1.0"

  @spec reset(t) :: {:ok, t} | {:error, error}

  def reset(t)

  def reset(%__MODULE__{} = status) do
    status |> update_entries(@harnais_error_status_value_entries_initial)
  end

  @doc false

  @since "0.1.0"

  @spec add_entries(t, any) :: result

  defp add_entries(%__MODULE__{@harnais_error_field_entries => entries} = state, values) do
    with {:ok, new_entries} <- values |> validate_results do
      entries =
        entries
        |> case do
          x when is_empty_list(x) -> new_entries
          x when is_filled_list(x) -> x ++ new_entries
        end

      state |> update_entries(entries)
    else
      {:error, %{__exception__: true}} = result -> result
    end
  end

  @doc false

  @since "0.1.0"

  @spec transform_entries(t, any) :: result

  defp transform_entries(t_or_entries, transform)

  defp transform_entries(%__MODULE__{@harnais_error_field_entries => entries}, fun_transform) do
    entries
    |> transform_entries(fun_transform)
  end

  defp transform_entries(entries, fun_transform)
       when is_function(fun_transform, 1) do
    entries
    |> case do
      x when is_filled_list(x) ->
        x
        |> map_collate0_enum(fun_transform)

      _ ->
        {:ok, @harnais_error_status_value_empty_return}
    end
  end

  defp transform_entries(_entries, fun_transform) do
    HEE.new_error_result(m: "entries transform invalid", v: fun_transform)
  end

  defp add_entry(%__MODULE__{} = status, entry) do
    status |> add_entries(entry |> List.wrap())
  end

  defp normalise_result(entry)

  defp normalise_result({:ok, _} = k) do
    {:ok, k}
  end

  defp normalise_result({:error, value} = r) do
    value
    |> Exception.exception?()
    |> case do
      true ->
        {:ok, r}

      _ ->
        with {:ok, error} <- new_anon_error(value) do
          {:ok, {:error, error}}
        else
          {:error, %{__exception__: true}} = result -> result
        end
    end
  end

  defp normalise_result(:ok) do
    {:ok, {:ok, :ok}}
  end

  defp normalise_result(:error) do
    with {:ok, error} <- new_anon_error(:error) do
      {:ok, {:error, error}}
    else
      {:error, %{__exception__: true}} = result -> result
    end
  end

  defp normalise_result(value) do
    HEE.new_error_result(m: "result invalid", v: value)
  end

  defp export_result_default_transform(result)

  defp export_result_default_transform({:ok, value}) do
    {:ok, [ok: value]}
  end

  defp export_result_default_transform({:error, %HEE{} = error}) do
    error
    |> HEE.export()
  end

  defp export_result_default_transform({:error, error}) do
    {:ok, [error: [[m: error |> Exception.message()]]]}
  end

  @doc ~S"""
  `export/2` takes an instance of the module's `struct` and optional *opts* and exports
  it.

  If the *opts* are not empty, `update/2` is called with the `struct`
  and `opts` before performing the export.

  A custom arity one export function can be given in the field
  `:export_function` and will be passed the `struct`.

  Otherwise the default export function is used which exports each
  entry and merges the answers.

  Exporting an `:ok` result returns `[ok: value]`.

  If an `:error` entry is not either a `Harnais.Error` or
  `Harnais.Error.Status`, the exports calls `Exception.message/1` and returns `[error: message]`.

  If the export works, `{:ok, export}` is returned.

  ## Examples

  This example shows the default exporter:

      iex> new!(add_results: [ok: 42, error: :got_an_error, error: %BadMapError{term: 42}])
      ...> |> export
      {:ok, [ok: 42, error: [[v: :got_an_error]], error: [[m: "expected a map, got: 42"]]]}

  This custom exporter counts the `:ok` and `:error` entries:

      iex> fun = fn %{entries: entries} ->
      ...>   entries
      ...>   |> case do
      ...>       x when is_list(x) ->
      ...>         x
      ...>         |> Enum.split_with(fn {verb,_} -> verb == :ok end)
      ...>         |> case do
      ...>              {oks,errors} -> {:ok, {oks |> length, errors |> length}}
      ...>            end
      ...>       _ -> {:ok, {0,0}}
      ...>     end
      ...> end
      ...> new!(
      ...>   export_function: fun,
      ...>   add_results: [ok: 42, error: :got_an_error, error: %BadMapError{term: 42}])
      ...> |> export
      {:ok, {1, 2}}

  """

  @since "0.1.0"

  @spec export(t, opts) :: result

  def export(t, opts \\ [])

  def export(%__MODULE__{@harnais_error_field_export_function => fun_export} = state, []) do
    fun_export
    |> case do
      fun when is_function(fun, 1) ->
        state |> fun.()

      fun when is_value_unset(fun) ->
        state
        |> transform_entries(&export_result_default_transform/1)
        |> case do
          {:error, %{__struct__: _}} = result ->
            result

          {:ok, exports} ->
            exports = exports |> Enum.reduce([], fn v, s -> s ++ v end)

            {:ok, exports}
        end

      fun ->
        new_error_result(m: "export function invalid", v: fun)
    end
  end

  def export(%__MODULE__{} = state, opts) do
    with {:ok, state} <- state |> update(opts),
         {:ok, _export} = result <- state |> export do
      result
    else
      {:error, %{__exception__: true}} = result -> result
    end
  end

  @doc false

  @since "0.1.0"

  def add_maybe_error_result(t, result, transforms \\ [])

  def add_maybe_error_result(status, result, transforms) do
    # any transforms to apply?
    transforms
    |> list_wrap_flat_just
    |> case do
      [] ->
        result

      funs ->
        funs
        |> Enum.reduce(result, fn f, r -> f.(r) end)
    end
    |> case do
      # only add if an error
      {:error, _} = r ->
        status |> add_result(r)

      _ ->
        {:ok, status}
    end
  end

  @doc ~S"""
  `add_results/2` adds new *results* to the existing *results*.

  `add_results/2` normalises any `{:error, value}` *results*, creating a new `Harnais.Error` `Exception` if the value is not an `Exception` already.

  ## Examples

      iex> {:ok, status} = new!() |> add_results(ok: 42, error: :got_an_error, ok: :good_one)
      ...> status |> get_results!
      ...> |> Enum.map(fn
      ...>    {:ok, _} = result -> result
      ...>    {:error, error} -> {:error, error |> Exception.message}
      ...> end)
      [ok: 42, error: ":got_an_error", ok: :good_one]

      iex> new!(add_results: [ok: 42, error: :got_an_error, error: %BadMapError{term: 42}])
      ...> |> get_results!
      ...> |> Enum.map(fn
      ...>    {:ok, _} = result -> result
      ...>    {:error, error} -> {:error, error |> Exception.message}
      ...> end)
      [ok: 42, error: ":got_an_error", error: "expected a map, got: 42"]

      iex> {:ok, status} = new!() |> add_results(ok: 42, error: :got_an_error, ok: :good_one)
      ...> status |> export!
      [ok: 42, error: [[v: :got_an_error]], ok: :good_one]

      iex> {:ok, status} = new!() |> add_results(ok: 42)
      ...> {:ok, status} = status |> add_results(error: :got_an_error, ok: :good_one)
      ...> status |> export!
      [ok: 42, error: [[v: :got_an_error]], ok: :good_one]

  """

  @spec add_results(t, results) :: {:ok, t} | {:error, error}

  @since "0.1.0"

  def add_results(t, results \\ [])

  def add_results(%__MODULE__{} = status, []) do
    {:ok, status}
  end

  def add_results(%__MODULE__{} = state, results) when is_list(results) do
    results
    |> map_collate0_enum(&normalise_result/1)
    |> case do
      {:error, %{__exception__: true}} = result ->
        result

      {:ok, results} ->
        state |> add_entries(results)
    end
  end

  @doc ~S"""
  `add_oks/2` adds a list of *oks* to the existing *results*.

  ## Examples

      iex> new!(add_oks: [42, :got_an_error, :good_one])
      ...> |> export!
      [ok: 42, ok: :got_an_error, ok: :good_one]

  """

  @since "0.1.0"

  @spec add_oks(t, oks) :: {:ok, t} | {:error, error}

  def add_oks(t, values)

  def add_oks(%__MODULE__{} = status, []) do
    {:ok, status}
  end

  def add_oks(%__MODULE__{} = state, oks) when is_list(oks) do
    results =
      oks
      |> Enum.map(fn ok -> {@harnais_error_status_type_k, ok} end)

    state |> add_results(results)
  end

  @doc ~S"""
  `add_errors/2` adds a list of *errors* to the existing *results*.

  ## Examples

      iex> new!(add_errors: [42, :got_an_error, :good_one])
      ...> |> export!
      [error: [[v: 42]], error: [[v: :got_an_error]], error: [[v: :good_one]]]

  """

  @since "0.1.0"

  @spec add_errors(t, errors) :: {:ok, t} | {:error, error}

  def add_errors(t, results)

  def add_errors(%__MODULE__{} = status, []) do
    {:ok, status}
  end

  def add_errors(%__MODULE__{} = state, errors) when is_list(errors) do
    results =
      errors
      |> Enum.map(fn error -> {@harnais_error_status_type_e, error} end)

    state |> add_results(results)
  end

  @doc ~S"""
  `add_result/2` calls `add_results/2` with the `List.wrap/1`-ed *result*.

  ## Examples

      iex> {:ok, t} = new!()
      ...> |> add_result!({:ok, 42})
      ...> |> add_result!(:error)
      ...> |> add_result(:ok)
      ...> t |> get_results!
      ...> |> Enum.map(fn
      ...>    {:ok, _} = result -> result
      ...>    {:error, error} -> {:error, error |> Exception.message}
      ...> end)
      [ok: 42, error: ":error", ok: :ok]

  """

  @since "0.1.0"

  @spec add_result(t, result) :: {:ok, t} | {:error, error}

  def add_result(t, result)

  def add_result(%__MODULE__{} = state, result) do
    with {:ok, %__MODULE__{}} = result <- state |> add_results(result |> List.wrap()) do
      result
    else
      {:error, %{__exception__: true}} = result -> result
    end
  end

  @doc ~S"""
  `add_ok/2` adds a single *ok* value to the existing *results*.

  ## Examples

      iex> {:ok, status} = new!() |> add_ok(42)
      ...> status |> export!
      [ok: 42]

  """

  @since "0.1.0"

  @spec add_ok(t, ok) :: {:ok, t} | {:error, error}

  def add_ok(t, ok)

  def add_ok(%__MODULE{} = state, value) do
    state |> add_oks([value])
  end

  @doc ~S"""
  `add_error/2` adds a single *error* to the existing *results*.

  ## Examples

      iex> {:ok, status} = new!() |> add_error(42)
      ...> status |> get_results!
      ...> |> List.first |> elem(1) |> Exception.message
      "42"

  """

  @since "0.1.0"

  @spec add_error(t, error) :: {:ok, t} | {:error, error}

  def add_error(t, error)

  def add_error(%__MODULE__{} = status, value) do
    status |> add_errors([value])
  end

  @doc ~S"""
  `get_oks/2` returns `{:ok, oks}`.

  ## Examples

      iex> {:error, error} = new!() |> get_oks
      ...> error |> Exception.message
      "Harnais.Error.Status has no oks"

      iex> new!()
      ...> |> add_result!({:ok, 42})
      ...> |> add_result!({:error, :got_an_error})
      ...> |> add_result!({:ok, :good_one})
      ...> |> get_oks
      {:ok, [42, :good_one]}

  """

  @since "0.1.0"

  @spec get_oks(t) :: result

  def get_oks(t)

  def get_oks(%__MODULE__{} = state) do
    with {:ok, results} <- state |> get_results do
      results
      |> Stream.filter(fn
        {@harnais_error_status_type_k, _v} -> true
        _ -> false
      end)
      |> Enum.map(&elem(&1, 1))
      |> case do
        x when is_filled_list(x) ->
          {:ok, x}

        _ ->
          new_error_result_no_oks()
      end
    else
      {:error, %{__struct__: _}} -> new_error_result_no_oks()
    end
  end

  @doc ~S"""
  `get_errors/2` returns `{:ok, errors}`.

  ## Examples

       iex> {:error, error} = new!() |> get_errors
       ...> error |> Exception.message
       "Harnais.Error.Status has no errors"

       iex> {:ok, errors} = new!()
       ...> |> add_result!({:ok, 42})
       ...> |> add_result!({:error, %BadMapError{term: 42}})
       ...> |> add_result!({:error, :got_an_error})
       ...> |> add_result!({:ok, :good_one})
       ...> |> get_errors
       ...> errors |> Enum.map(&Exception.message/1)
       ["expected a map, got: 42", ":got_an_error"]

  """

  @since "0.1.0"

  @spec get_errors(t) :: result

  def get_errors(t)

  def get_errors(%__MODULE__{} = state) do
    with {:ok, results} <- state |> get_results do
      results
      |> Stream.filter(fn
        {@harnais_error_status_type_e, _v} -> true
        _ -> false
      end)
      |> Enum.map(&elem(&1, 1))
      |> case do
        x when is_filled_list(x) ->
          {:ok, x}

        _ ->
          new_error_result_no_errors()
      end
    else
      {:error, %{__struct__: _}} -> new_error_result_no_errors()
    end
  end

  @doc ~S"""
  `get_results/1` returns the complete collection of *results* as `{:ok, results}`.

  ## Examples

      iex> {:error, error} = new!() |> get_results
      ...> error |> Exception.message
      "Harnais.Error.Status has no results"

      iex> {:ok, status} = new!() |> add_results([ok: 42, error: :got_an_error, ok: :good_one])
      ...> status |> export!
      [ok: 42, error: [[v: :got_an_error]], ok: :good_one]

  """

  @since "0.1.0"

  @spec get_results(t) :: result

  def get_results(t)

  def get_results(%__MODULE__{@harnais_error_field_entries => entries}) do
    entries
    |> case do
      x when is_filled_list(x) ->
        {:ok, x}

      _ ->
        new_error_result_no_results()
    end
  end

  @doc ~S"""
  `get_last_ok/1` returns `{:ok, ok}` where `ok` is the last *ok*.

  ## Examples

       iex> {:error, error} = new!() |> get_last_ok
       ...> error |> Exception.message
       "Harnais.Error.Status has no oks"

       iex> new!()
       ...> |> add_result!({:ok, 42})
       ...> |> add_result!({:error, :got_an_error})
       ...> |> add_result!({:ok, :good_one})
       ...> |> get_last_ok!
       :good_one

  """

  @since "0.1.0"

  @spec get_last_ok(t) :: result

  def get_last_ok(t)

  def get_last_ok(%__MODULE__{} = status) do
    with {:ok, oks} <- status |> get_oks do
      {:ok, oks |> List.last()}
    else
      {:error, %{__exception__: true}} = result -> result
    end
  end

  @doc ~S"""
  `get_last_ok_result/2` returns `{:ok, result}` where `result` is the last *ok* *result*.

  ## Examples

      iex> {:error, error} = new!() |> get_last_ok_result
      ...> error |> Exception.message
      "Harnais.Error.Status has no oks"

      iex> new!()
      ...> |> add_result!({:ok, 42})
      ...> |> add_result!({:error, :got_an_error})
      ...> |> add_result!({:ok, :good_one})
      ...> |> get_last_ok_result!
      {:ok, :good_one}

  """

  @since "0.1.0"

  @spec get_last_ok_result(t) :: result

  def get_last_ok_result(t)

  def get_last_ok_result(%__MODULE__{} = status) do
    with {:ok, oks} <- status |> get_oks do
      {:ok, {:ok, oks |> List.last()}}
    else
      {:error, %{__exception__: true}} = result -> result
    end
  end

  @doc ~S"""
  `get_last_error/2` returns the last *ok* *result* in the *results*.

  If there are no results, it returns `nil`.

  ## Examples

      iex> {:error, error} = new!() |> get_last_error
      ...> error |> Exception.message
      "Harnais.Error.Status has no errors"

      iex> new!()
      ...> |> add_result!({:ok, 42})
      ...> |> add_result!({:error, :got_an_error})
      ...> |> add_result!({:ok, :good_one})
      ...> |> get_last_error!
      ...> |> Exception.message
      ":got_an_error"

  """

  @since "0.1.0"

  @spec get_last_error(t) :: nil | {:error, error}

  def get_last_error(t)

  def get_last_error(%__MODULE__{} = status) do
    with {:ok, errors} <- status |> get_errors do
      {:ok, errors |> List.last()}
    else
      {:error, %{__exception__: true}} = result -> result
    end
  end

  @doc ~S"""
  `get_last_error_result/2` returns a the last `{:ok, value}` tuple in the results.

  If there are no results, it returns `nil`.

  ## Examples

       iex> {:error, error} = new!() |> get_last_error_result
       ...> error |> Exception.message
       "Harnais.Error.Status has no errors"

       iex> {:ok, error} = new!()
       ...> |> add_result!({:ok, 42})
       ...> |> add_result!({:error, :got_an_error})
       ...> |> add_result!({:ok, :good_one})
       ...> |> get_last_error_result
       ...> error |> Exception.message
       ":got_an_error"

  """

  @since "0.1.0"

  @spec get_last_error_result(t) :: nil | {:error, error}

  def get_last_error_result(t)

  def get_last_error_result(%__MODULE__{} = status) do
    with {:ok, errors} <- status |> get_errors do
      {:ok, errors |> List.last()}
    else
      {:error, %{__exception__: true}} = result -> result
    end
  end

  @doc ~S"""
  `get_last_result/2` returns a the last `{:ok, value}` or `{:error, value}` tuple in the results.

  If there are no results, it returns `nil`.

  ## Examples

      iex> {:error, error} = new!() |> get_last_result
      ...> error |> Exception.message
      "Harnais.Error.Status has no results"

      iex> new!()
      ...> |> add_result!({:ok, 42})
      ...> |> add_result!({:error, :got_an_error})
      ...> |> add_result!({:ok, :good_one})
      ...> |> get_last_result!
      {:ok, :good_one}

  """

  @since "0.1.0"

  @spec get_last_result(t) :: nil | {:ok, ok} | {:error, error}

  def get_last_result(t)

  def get_last_result(%__MODULE__{} = status) do
    with {:ok, results} <- status |> get_results do
      {:ok, results |> List.last()}
    else
      {:error, %{__exception__: true}} = result -> result
    end
  end

  @doc ~S"""
  `has_oks?/1` returns `true` if the status has any `:ok` results, else `false`.

  ## Examples

      iex> new!() |> has_oks?
      false

      iex> new!() |> add_error!(:error1) |> add_ok!(:ok1) |> has_oks?
      true

  """

  @since "0.1.0"

  @spec has_oks?(t) :: true | false

  def has_oks?(t)

  def has_oks?(%__MODULE__{
        @harnais_error_field_entries => @harnais_error_status_value_entries_initial
      }) do
    false
  end

  def has_oks?(%__MODULE__{@harnais_error_field_entries => entries}) do
    entries
    |> Enum.any?(fn
      {@harnais_error_status_type_k, _} -> true
      _ -> false
    end)
  end

  @doc ~S"""
  `has_errors?/1` returns `true` if the status has any `:ok` results, else `false`.

  ## Examples

      iex> new!() |> has_errors?
      false

      iex> new!() |> add_error!(:error1) |> add_ok!(:ok1) |> has_errors?
      true

  """

  @since "0.1.0"

  @spec has_errors?(t) :: true | false

  def has_errors?(t)

  def has_errors?(%__MODULE__{
        @harnais_error_field_entries => @harnais_error_status_value_entries_initial
      }) do
    false
  end

  def has_errors?(%__MODULE__{@harnais_error_field_entries => entries}) do
    entries
    |> Enum.any?(fn
      {@harnais_error_status_type_e, _} -> true
      _ -> false
    end)
  end

  @doc ~S"""
  `has_errors?/1` returns `true` if the status has any results, else `false`.

  ## Examples

      iex> new!() |> has_results?
      false

      iex> new!() |> add_error!(:error1) |> add_ok!(:ok1) |> has_results?
      true

  """

  @since "0.1.0"

  @spec has_results?(t) :: true | false

  def has_results?(t)

  def has_results?(%__MODULE__{
        @harnais_error_field_entries => @harnais_error_status_value_entries_initial
      }) do
    false
  end

  def has_results?(%__MODULE__{@harnais_error_field_entries => _entries}) do
    true
  end

  @doc_message ~S"""
  `message/1` is the standard `Exception` callback.

  ## Examples

       iex> new!() |> message
       "Harnais.Error.Status has no errors"

       iex> new!()
       ...> |> add_result!({:ok, 42})
       ...> |> add_result!({:error, :got_an_error})
       ...> |> add_result!({:ok, :good_one})
       ...> |> message
       ":got_an_error"

       iex> new!()
       ...> |> add_result!({:ok, 42})
       ...> |> add_result!({:error, :got_an_error})
       ...> |> add_result!({:ok, :good_one})
       ...> |> add_result!({:error, :another_error})
       ...> |> message
       ":got_an_error; :another_error"

  """

  [
    # update the doc, etc proxies
    {@plymio_codi_pattern_proxy_put,
     [
       defexception_def_message_doc: quote(do: @doc(unquote(@doc_message))),
       defexception_def_message_since: "0.1.0",
       defexception_def_message_header: quote(do: def(message(t))),
       defexception_def_message_spec: quote(do: @spec(message(t) :: String.t())),
       defexception_def_format_error_message_doc: :doc_false,
       defexception_def_format_error_message_since: "0.1.0"
     ]},
    {@plymio_codi_pattern_proxy_fetch,
     [
       :defexception_redtape,
       :defexception_types,
       :defexception_def_new_result,
       :doc_false,
       :defexception_def_new_error_result_defdelegate_new_result,
       :defexception_def_message_doc,
       :defexception_def_message_since,
       :defexception_def_message_spec,
       :defexception_def_message_header,
       :defexception_def_message_clause_user_transform
     ]}
  ]
  |> CODI.reify_codi(@codi_opts)

  def message(%__MODULE__{@harnais_error_field_entries => entries})
      when not is_filled_list(entries) do
    "Harnais.Error.Status has no errors"
  end

  def message(%__MODULE__{} = status) do
    with {:ok, errors} <- status |> get_errors do
      errors
      |> case do
        errors when is_filled_list(errors) ->
          with {:ok, texts} <-
                 errors
                 |> map_collate0_enum(&format_error_message_value/1) do
            texts |> Enum.join("; ")
          else
            {:error, %{__exception__: true}} = result -> result
          end

        _ ->
          "Harnais.Error.Status has no errors"
      end
    else
      {:error, _error} -> "Harnais.Error.Status has no errors"
    end
  end

  defp new_anon_error(value) do
    HEE.new(v: value)
  end

  @doc false

  @since "0.1.0"

  def transform(status, transforms \\ [])

  def transform(%__MODULE__{} = status, []) do
    status
  end

  def transform(%__MODULE__{} = status, transforms) do
    transforms
    |> list_wrap_flat_just
    |> Enum.reduce(status, fn f, %__MODULE__{} = s -> f.(s) end)
  end

  @doc false

  @since "0.1.0"

  # reduces a status until the predicate is true
  # note: predciate check is done first so reduction can be preempted

  def reduce_until(status, fun_predicate, fun_reduce)

  def reduce_until(%__MODULE__{} = status, fun_predicate, fun_reduce)
      when is_function(fun_predicate) and is_function(fun_reduce) do
    fun_predicate =
      fun_predicate
      |> case do
        x when is_function(x, 2) -> x
        x when is_function(x, 1) -> fn _v, s -> fun_predicate.(s) end
      end

    fun_reduce
    |> list_wrap_flat_just
    |> Enum.reduce_while(
      status,
      fn f, %__MODULE__{} = s ->
        # continue if not true
        case fun_predicate.(f, s) do
          x when x in [nil, false] ->
            case f.(s) do
              %__MODULE__{} = new_s -> {:cont, new_s}
              x when is_nil(x) -> {:cont, s}
              x -> {:cont, s |> add_result(x)}
            end

          _ ->
            {:halt, s}
        end
      end
    )
  end

  quote_type_t = quote(do: t())
  quote_type_any = quote(do: any())

  quote_type_ok = quote(do: ok)
  quote_type_error = quote(do: error)
  quote_type_result = quote(do: result)
  quote_type_entry = quote(do: result)

  quote_type_oks = quote(do: oks)
  quote_type_errors = quote(do: errors)

  quote_arg_status = quote(do: t)
  quote_arg_ok = quote(do: ok)
  quote_arg_error = quote(do: error)
  quote_arg_result = quote(do: result)
  quote_arg_entry = quote(do: entry)

  quote_arg_oks = quote(do: oks)
  quote_arg_errors = quote(do: errors)
  quote_arg_results = quote(do: results)
  quote_arg_entries = quote(do: entries)

  [
    add_ok: [
      type: [args: [quote_type_t, quote_type_ok], result: quote_type_result],
      args: [quote_arg_status, quote_arg_ok]
    ],
    add_error: [
      type: [args: [quote_type_t, quote_type_error], result: quote_type_result],
      args: [quote_arg_status, quote_arg_error]
    ],
    add_result: [
      type: [args: [quote_type_t, quote_type_any], result: quote_type_result],
      args: [quote_arg_status, quote_arg_result]
    ],
    add_entry: [
      type: [args: [quote_type_t, quote_type_any], result: quote_type_entry],
      args: [quote_arg_status, quote_arg_entry]
    ],
    add_oks: [
      type: [args: [quote_type_t, quote_type_oks], result: quote_type_result],
      args: [quote_arg_status, quote_arg_oks]
    ],
    add_errors: [
      type: [args: [quote_type_t, quote_type_errors], result: quote_type_result],
      args: [quote_arg_status, quote_arg_errors]
    ],
    add_results: [
      type: [args: [quote_type_t, quote_type_any], result: quote_type_result],
      args: [quote_arg_status, quote_arg_results]
    ],
    add_entries: [
      type: [args: [quote_type_t, quote_type_any], result: quote_type_result],
      args: [quote_arg_status, quote_arg_entries]
    ],
    get_oks: [
      type: [args: [quote_type_t], result: quote_type_result],
      args: [quote_arg_status]
    ],
    get_errors: [
      type: [args: [quote_type_t], result: quote_type_result],
      args: [quote_arg_status]
    ],
    get_results: [
      type: [args: [quote_type_t], result: quote_type_result],
      args: [quote_arg_status]
    ],
    get_last_ok: [
      type: [args: [quote_type_t], result: quote_type_result],
      args: [quote_arg_status]
    ],
    get_last_error: [
      type: [args: [quote_type_t], result: quote_type_result],
      args: [quote_arg_status]
    ],
    get_last_result: [
      type: [args: [quote_type_t], result: quote_type_result],
      args: [quote_arg_status]
    ],
    get_last_ok_result: [
      type: [args: [quote_type_t], result: quote_type_result],
      args: [quote_arg_status]
    ],
    get_last_error_result: [
      type: [args: [quote_type_t], result: quote_type_result],
      args: [quote_arg_status]
    ],
    export: [
      type: [args: [quote_type_t], result: quote_type_result],
      args: [quote_arg_status]
    ],
    reset: [
      type: [args: [quote_type_t], result: quote(do: {:ok, t})],
      args: [quote_arg_status]
    ]
  ]
  |> Enum.map(fn {k, v} ->
    {@plymio_codi_key_pattern,
     v
     |> Keyword.put(@plymio_codi_key_bang_name, k)
     |> Keyword.put(@plymio_codi_key_pattern, @plymio_codi_pattern_bang)}
  end)
  |> CODI.reify_codi()
end

defimpl Inspect, for: Harnais.Error.Status do
  use Harnais.Error.Attribute

  def inspect(%Harnais.Error.Status{@harnais_error_field_entries => entries}, _opts) do
    case entries do
      x when x == @harnais_error_status_value_entries_initial ->
        "HES(X)"

      x when is_list(x) ->
        grouped_entries = entries |> Enum.group_by(fn {k, _v} -> k end)

        telltale_entries =
          [
            {@harnais_error_status_type_k, "K"},
            {@harnais_error_status_type_e, "E"}
          ]
          |> Enum.map(fn {type, mnemonic} ->
            case grouped_entries |> Map.has_key?(type) do
              true -> "#{mnemonic}=#{inspect(grouped_entries |> Map.get(type) |> length)}"
              _ -> "#{mnemonic}=X"
            end
          end)
          |> Enum.join("/")

        "HES(#{telltale_entries})"
    end
  end
end
