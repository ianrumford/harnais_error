defmodule Harnais.Error do
  @moduledoc ~S"""
  The Exception for the Harnais Package Family.

  Many of the functions in the Harnais packages return
  either `{:ok, any}` or `{error, error}` where `error` will be an
  `Exception`.

  Many of the errors will be instances of `Harnais.Error` `struct`.

  ## Exporting the Exception

  As well as supporting the usual `Exception` callbacks, the package
  support "exporting" the exception using `Harnais.Error.export/1`.

  See the exception fields `:export_function` and `:export_config` for
  how to contol what the export does.

  ## Exception State

  The exception `struct` has a number of fields

  | Field | Aliases |
  | :---------------- | -------------------: |
  | `:message`   | *:m, :msg* |
  | `:reason`   | *:r* |
  | `:type`   | *:t* |
  | `:location` | *:l, :loc, :ndx, :index, :i* |
  | `:value0` | *:v, :v0, :value, :e, :error* |
  | `:value1` | *:v1* |
  | `:value2` | *:v2* |
  | `:message_function` |  |
  | `:message_config` |  |
  | `:export_function` |  |
  | `:export_config` |  |

  ### Exception Field: `:message`

  The explanatory message, normally a string.

  ### Exception Field: `:reason`

  The reason for the error; normally an `Atom`.

  ### Exception Field: `:type`

  An aribtrary term defining the type of the error e.g. `:arg`, `:key`, `:value`, etc

  ### Exception Field: `:location`

  An aribtrary term defining where the error happened.

  For example this could be a key, index in a list, whatever.

  ### Exception Field: `:value0`, `:value1`, `:value2`

  Arbitrary terms identifying the cause of the error.

  Three fields are useful when, for example, `:value0` holds the name of
  a key and `:value1` and `:value2` hold the values of the key that
  were expected to be equal.

  Sometimes `:value0` holds an instance of `Harnais.Error.Status`.

  ### Exception Field: `:message_function`

  The module implements its own `Exception.message/1` callback but
  this can be overridden by this field.

  If supplied, it must be an arity 1 function that is passed the
  `struct` and must return a string.

  ### Exception Field: `:message_config`

  This field is used by the default `Exception.message/1` formatter to
  hold the fields to be included in the message.  It can be overridden
  to set the wanted fields.

  If an explicit `:message_function` function is supplied, this field
  can also be used to hold configuration for it.

  ### Exception Field: `:export_function`

  The exception can be "exported" using `Harnais.Error.export/1` which
  is passed the exception struct.

  The default exporter creates a `Keyword` of selected fields where
  the keys are the *shortest* alias for the field (e.g. `:m` for
  `:message`).

  A custom export function can be provided using this field.

  ### Exception Field: `:export_config`

  The default exporter using this field to hold the fields to be
  included in the export.

  If a custom export function is provided, this field can be used to
  provide configuration for it.

  ## Standard API

  Unless otherwise specified, functions return either `{:ok, status}`
  or `{:error, error}` when `error` will be an Exception..

  Many functions have peer *bang* functions that returns either the `value`
  in `{:ok, value}` or raise the `error` in `{:error, error}`.
  """

  require Logger
  require Plymio.Codi, as: CODI
  require Plymio.Fontais.Option
  alias Harnais.Utility, as: HUU
  alias Harnais.Error.Status, as: HES
  use Plymio.Fontais.Attribute
  use Plymio.Codi.Attribute
  use Harnais.Attribute
  use Harnais.Error.Attribute

  @codi_opts [
    {@plymio_codi_key_vekil, Plymio.Vekil.Codi.__vekil__()}
  ]

  import Plymio.Fontais.Guard,
    only: [
      is_value_set: 1,
      is_value_unset: 1
    ]

  import Plymio.Fontais.Utility,
    only: [
      list_wrap_flat_just: 1
    ]

  import Plymio.Fontais.Option,
    only: [
      opts_create_aliases_dict: 1,
      opts_canonical_keys: 2,
      canonical_key: 2
    ]

  import Plymio.Fontais.Error.Utility,
    only: [
      reduce_errors_default_function: 1
    ]

  @harnais_error_kvs_aliases [
    @harnais_error_field_alias_message,
    @harnais_error_field_alias_reason,
    @harnais_error_field_alias_type,
    @harnais_error_field_alias_location,
    @harnais_error_field_alias_value0,
    @harnais_error_field_alias_value1,
    @harnais_error_field_alias_value2,
    @harnais_error_field_alias_message_function,
    @harnais_error_field_alias_export_function,
    @harnais_error_field_alias_message_config,
    @harnais_error_field_alias_export_config
  ]

  @harnais_error_dict_aliases @harnais_error_kvs_aliases
                              |> opts_create_aliases_dict

  @harnais_error_kvs_short_aliases @harnais_error_kvs_aliases
                                   |> Enum.map(fn {canon_key, alias_keys} ->
                                     alias_keys = alias_keys |> list_wrap_flat_just

                                     all_keys = [canon_key | alias_keys]

                                     alias_shortest =
                                       [canon_key | alias_keys]
                                       |> Enum.sort_by(fn key ->
                                         key |> to_string |> String.length()
                                       end)
                                       |> List.first()

                                     {alias_shortest, all_keys}
                                   end)

  @harnais_error_dict_short_aliases @harnais_error_kvs_short_aliases
                                    |> opts_create_aliases_dict

  @doc false

  def update_canonical_opts(opts, dict \\ @harnais_error_dict_aliases) do
    opts |> opts_canonical_keys(dict)
  end

  @doc false

  def opts_short_keys(opts, dict \\ @harnais_error_dict_short_aliases) do
    opts |> opts_canonical_keys(dict)
  end

  @doc false

  def short_key(key, dict \\ @harnais_error_dict_short_aliases) do
    key |> canonical_key(dict)
  end

  @harnais_error_defstruct @harnais_error_kvs_aliases
                           |> Enum.map(fn {k, _v} -> {k, @plymio_fontais_the_unset_value} end)
                           |> Kernel.++([
                             {@harnais_error_field_message_config,
                              @harnais_error_default_message_config},
                             {@harnais_error_field_export_config,
                              @harnais_error_default_export_config}
                           ])
                           |> Keyword.new()

  defexception @harnais_error_defstruct

  @type t :: %__MODULE__{}
  @type opts :: Harnais.opts()
  @type kv :: {any, any}
  @type error :: Harnais.error()

  @doc_new ~S"""

  `new/1` creates an instance of the error module's struct `t`.

  If the `opts` are not empty, it calls `update/2` with `t` and the `opts`.

  Either `{:ok, t}` or `{:error, error}` is returned where `error` is
  an exception generated during the creation.

  ## Examples

      iex> {:ok, t} = new()
      ...> match?(%Harnais.Error{}, t)
      true

      iex> {:ok, t} = new(m: "failed again", r: :usual_cause, v: 42)
      ...> t |> Exception.message
      "failed again, reason=:usual_cause, got: 42"
  """

  [
    # update the doc, etc proxies
    {@plymio_codi_pattern_proxy_put,
     [
       state_def_new_since: quote(do: @since("0.1.0")),
       state_def_new_since!: quote(do: @since("0.1.0")),
       state_def_update_since: quote(do: @since("0.1.0")),
       state_def_update_since!: quote(do: @since("0.1.0")),
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
       defexception_def_message_doc:
         quote do
           @doc ~S"""
           `message/1` is the standard `Exception` callback.
           """
         end,
       defexception_def_message_since: quote(do: @since("0.1.0")),
       defexception_def_message_header: quote(do: def(message(t))),
       defexception_def_message_spec: quote(do: @spec(message(t) :: String.t())),

       # this is set to nil in fontais v0.2.0 so function is visible
       defexception_def_format_error_message_doc: :doc_false,
       defexception_def_format_error_message_since: quote(do: @since("0.1.0")),

       # this is set to nil in fontais v0.2.0 so function is visible
       defexception_def_new_result_doc: :doc_false,
       defexception_def_new_result_since: quote(do: @since("0.1.0"))
     ]},
    {@plymio_codi_pattern_proxy_put,
     [
       state_defp_update_field_passthru:
         quote do
           defp update_field(%__MODULE__{} = state, {k, v}) do
             state
             |> Map.has_key?(k)
             |> case do
               true ->
                 {:ok, state |> struct!([{k, v}])}

               _ ->
                 new_error_result(m: "field invalid", v: k)
             end
           end
         end
     ]},
    {@plymio_codi_pattern_proxy_fetch,
     [
       :defexception_redtape,
       :defexception_def_new_result,
       :doc_false,
       :defexception_def_new_error_result_defdelegate_new_result,
       :defexception_def_message_doc,
       :defexception_def_message_since,
       :defexception_def_message_spec,
       :defexception_def_message_header,
       :defexception_def_message_clause_user_transform,
       :defexception_def_message_clause_default,
       :defexception_def_format_error_message,
       :state_base_package,
       :state_defp_update_field_header
     ]},
    {@plymio_codi_pattern_proxy_fetch,
     [
       {@plymio_codi_key_proxy_name, :state_defp_update_proxy_field_fun1},
       {@plymio_fontais_key_forms_edit,
        [{@plymio_fontais_key_rename_atoms, [proxy_field: @harnais_error_field_message_function]}]}
     ]},
    {@plymio_codi_pattern_proxy_fetch,
     [
       {@plymio_codi_key_proxy_name, :state_defp_update_proxy_field_fun1},
       {@plymio_fontais_key_forms_edit,
        [{@plymio_fontais_key_rename_atoms, [proxy_field: @harnais_error_field_export_function]}]}
     ]},
    {@plymio_codi_pattern_proxy_fetch, [:state_defp_update_field_passthru]}
  ]
  |> CODI.reify_codi(@codi_opts)

  @doc false

  @since "0.1.0"

  def new_errors_result(errors) do
    with {:ok, error} <- errors |> reduce_errors_default_function do
      {:error, error}
    else
      {:error, %{__exception__: true}} = result -> result
    end
  end

  @since "0.1.0"

  def exception(opts \\ [])

  def exception(%__MODULE__{} = error) do
    error
  end

  def exception(%HES{} = status) do
    exception(value: status, message: "status error", reason: :status)
  end

  def exception(opts) when is_list(opts) do
    opts
    |> new!
  end

  @doc false

  @since "0.1.0"

  def new_from_status(opts \\ [])

  def new_from_status(%HES{} = status) do
    {:ok, %__MODULE__{@harnais_error_field_value0 => status}}
  end

  def new_from_status(opts) when is_list(opts) do
    with {:ok, %HES{} = status} <- opts |> HES.new() do
      {:ok, %__MODULE__{@harnais_error_field_value0 => status}}
    else
      {:error, %{__exception__: true}} = result -> result
    end
  end

  @doc false

  def new_error_result_from_status(value) do
    with {:ok, error} <- value |> new_from_status do
      {:error, error}
    else
      {:error, %{__exception__: true}} = result -> result
    end
  end

  @doc false

  @since "0.1.0"

  def normalise_result(value)

  def normalise_result({:ok, _} = result) do
    result
  end

  def normalise_result({:error, %HES{} = status}) do
    {:error, exception(status)}
  end

  def normalise_result(%HES{} = status) do
    case status |> HES.has_errors?() do
      # return an exception
      true ->
        {:error, exception(status)}

      # if no errors, return the last ok result
      _ ->
        status |> HES.get_last_ok_result()
    end
  end

  def normalise_result({:error, errors}) when is_list(errors) do
    with {:ok, %HES{} = status} <- [add_errors: errors] |> HES.new() do
      {:error, exception(status)}
    else
      {:error, %{__exception__: true}} = result -> result
    end
  end

  def normalise_result(errors) when is_list(errors) do
    {:error, errors} |> normalise_result
  end

  def normalise_result({:error, %__MODULE__{}} = r) do
    r
  end

  def normalise_result(%__MODULE__{} = error) do
    {:error, error}
  end

  @doc false

  @since "0.1.0"

  # this is used in doctests
  def harnais_error_export_result(value)

  def harnais_error_export_result(%__MODULE__{} = error) do
    error |> export
  end

  def harnais_error_export_result(%HES{} = status) do
    status |> HES.harnais_status_export_result()
  end

  def harnais_error_export_result(
        {:error, %__MODULE__{@harnais_error_field_value0 => %HES{} = status}}
      ) do
    status |> HES.harnais_status_export_result()
  end

  def harnais_error_export_result({:error, %__MODULE__{} = error}) do
    error |> harnais_error_export_result
  end

  def harnais_error_export_result(%__MODULE__{@harnais_error_field_value0 => %HES{} = status}) do
    status |> HES.harnais_status_export_result()
  end

  defp default_export_function(result)

  defp default_export_function(
         %__MODULE__{@harnais_error_field_export_config => format_order} = state
       ) do
    with export <- state |> Map.from_struct(),
         export <- export |> Map.take(format_order),
         export <- export |> Map.to_list(),
         export <- export |> HUU.opts_sort_keys(format_order),
         export <- export |> Enum.filter(fn {_k, v} -> v |> is_value_set end),
         {:ok, export} <- export |> opts_short_keys,
         true <- true do
      {:ok, [error: [export]]}
    else
      {:error, %{__exception__: true}} = result -> result
    end
  end

  @doc ~S"""
  `export/1` takes an instance of the module's `struct` and exports it.

  A custom arity one export function can be given in the field `:export_function`.

  Otherwise the default export function is used which creates `Keyword`
  with one key (`:error`) whose value is a list, currently with only
  one entry which is itself a `Keyword` of the set fields in the
  *error* where the keys are the short form of the full field name (e.g. `:m` for `:message`)

  If the export works, `{:ok, export}` is returned.

  ## Examples

      iex> {:ok, error} = [message: "something broke", type: :arg, value: 42] |> new
      ...> error |> export
      {:ok, [error: [[m: "something broke", t: :arg, v: 42]]]}

      iex> export_fun = fn error -> {:ok, error.value} end
      ...> {:ok, error} = [export_function: export_fun, message: "something broke", type: :arg, value: 42] |> new
      ...> error |> export
      {:ok, 42}

  """

  @since "0.1.0"

  @spec export(t) :: {:ok, any} | {:error, error}

  def export(t)

  def export(%__MODULE__{@harnais_error_field_export_function => fun_export} = error) do
    fun_export
    |> case do
      fun when is_value_set(fun) ->
        error |> transform(fun)

      _ ->
        error |> transform(&default_export_function/1)
    end
  end

  @doc false

  @since "0.1.0"

  def transform(t, transform)

  def transform(%__MODULE__{} = error, fun_transform)
      when is_function(fun_transform, 1) do
    error
    |> fun_transform.()
  end
end

defimpl Inspect, for: Harnais.Error do
  use Harnais.Attribute
  use Harnais.Error.Attribute

  import Plymio.Fontais.Guard,
    only: [
      is_value_unset: 1
    ]

  def inspect(
        %Harnais.Error{
          @harnais_error_field_message => message,
          @harnais_error_field_reason => reason,
          @harnais_error_field_type => type,
          @harnais_error_field_location => location,
          @harnais_error_field_value0 => value0,
          @harnais_error_field_value1 => value1,
          @harnais_error_field_value2 => value2
        },
        _opts
      ) do
    message_telltale =
      message
      |> case do
        x when is_value_unset(x) -> nil
        _x -> "M=+"
      end

    reason_telltale =
      reason
      |> case do
        x when is_value_unset(x) -> nil
        x -> "R=#{inspect(x)}"
      end

    type_telltale =
      type
      |> case do
        x when is_value_unset(x) -> nil
        x -> "T=#{inspect(x)}"
      end

    location_telltale =
      location
      |> case do
        x when is_value_unset(x) -> nil
        x -> "L=#{inspect(x)}"
      end

    value0_telltale =
      value0
      |> case do
        x when is_value_unset(x) -> nil
        x -> "V0=#{inspect(x)}"
      end

    value1_telltale =
      value1
      |> case do
        x when is_value_unset(x) -> nil
        x -> "V1=#{inspect(x)}"
      end

    value2_telltale =
      value2
      |> case do
        x when is_value_unset(x) -> nil
        x -> "V2=#{inspect(x)}"
      end

    error_telltale =
      [
        message_telltale,
        reason_telltale,
        type_telltale,
        location_telltale,
        value0_telltale,
        value1_telltale,
        value2_telltale
      ]
      |> Enum.reject(&is_nil/1)
      |> Enum.join("; ")

    "HEE(#{error_telltale})"
  end
end
