defmodule Harnais.Error.Attribute do
  @moduledoc false

  defmacro __using__(_opts \\ []) do
    quote do
      use Plymio.Fontais.Attribute

      @harnais_error_status_value_no_key :no_key
      @harnais_error_status_value_no_value :no_value
      @harnais_error_status_value_entries_initial []
      @harnais_error_status_value_empty_return []
      @harnais_error_status_value_nil_return nil

      @harnais_error_value_field_type_arg :arg
      @harnais_error_value_field_type_key :key
      @harnais_error_value_field_type_value :value

      @harnais_error_key_error :error
      @harnais_error_key_status :status

      @harnais_error_field_message @plymio_fontais_error_field_message
      @harnais_error_field_reason @plymio_fontais_error_field_reason

      @harnais_error_field_type :type
      @harnais_error_field_location :location

      @harnais_error_field_value0 :value
      @harnais_error_field_value1 :value1
      @harnais_error_field_value2 :value2

      @harnais_error_field_message_function @plymio_fontais_error_field_message_function
      @harnais_error_field_message_config @plymio_fontais_error_field_message_config

      @harnais_error_field_export_function :export_function
      @harnais_error_field_export_config :export_config

      @harnais_error_field_entries :entries

      @harnais_error_field_alias_message @plymio_fontais_error_field_alias_message
      @harnais_error_field_alias_reason @plymio_fontais_error_field_alias_reason

      @harnais_error_field_alias_type {@harnais_error_field_type, [:t]}
      @harnais_error_field_alias_location {@harnais_error_field_location,
                                           [:l, :loc, :ndx, :i, :index]}

      # note short key is 'v' not 'v0'
      @harnais_error_field_alias_value0 {@harnais_error_field_value0,
                                         [:v, :v0, :value, :e, :error]}
      @harnais_error_field_alias_value1 {@harnais_error_field_value1, [:v1]}
      @harnais_error_field_alias_value2 {@harnais_error_field_value2, [:v2]}

      @harnais_error_field_alias_message_function @plymio_fontais_error_field_alias_message_function
      @harnais_error_field_alias_message_config @plymio_fontais_error_field_alias_message_config

      @harnais_error_field_alias_export_function {@harnais_error_field_export_function, nil}
      @harnais_error_field_alias_export_config {@harnais_error_field_export_config, nil}

      @harnais_error_status_verb_add_ok :add_ok
      @harnais_error_status_verb_add_error :add_error
      @harnais_error_status_verb_add_result :add_result
      @harnais_error_status_verb_add_oks :add_oks
      @harnais_error_status_verb_add_errors :add_errors
      @harnais_error_status_verb_add_results :add_results

      @harnais_error_status_verbs [
        @harnais_error_status_verb_add_ok,
        @harnais_error_status_verb_add_error,
        @harnais_error_status_verb_add_result,
        @harnais_error_status_verb_add_oks,
        @harnais_error_status_verb_add_errors,
        @harnais_error_status_verb_add_results
      ]

      @harnais_error_status_type_k :ok
      @harnais_error_status_type_e :error

      @harnais_error_reason_not_arity :not_arity
      @harnais_error_reason_not_number :not_number
      @harnais_error_reason_not_ast :not_ast
      @harnais_error_reason_not_map :not_map
      @harnais_error_reason_not_list :not_list
      @harnais_error_reason_not_keyword :not_keyword
      @harnais_error_reason_not_module :not_module
      @harnais_error_reason_not_module_fva :not_module_fva
      @harnais_error_reason_mismatch :mismatch
      @harnais_error_reason_missing :missing
      @harnais_error_reason_compare :compare

      @harnais_error_default_message_config [
        @harnais_error_field_message,
        @harnais_error_field_reason,
        @harnais_error_field_type,
        @harnais_error_field_location,
        @harnais_error_field_value1,
        @harnais_error_field_value2,

        # value0 needs to be last for "got: blah" to work in Error's message
        @harnais_error_field_value0
      ]

      @harnais_error_default_export_config [
        @harnais_error_field_message,
        @harnais_error_field_reason,
        @harnais_error_field_type,
        @harnais_error_field_location,
        @harnais_error_field_value0,
        @harnais_error_field_value1,
        @harnais_error_field_value2
      ]
    end
  end
end
