with ada.containers.vectors;
with ada.containers.indefinite_ordered_maps;
with ada.containers.indefinite_vectors;

with numbers; use numbers;
with strings; use strings;
with env; use env;

package getter.macros is
  ERROR_NO_CLOSED : exception;
  ERROR_NO_DEFINED : exception;
  ERROR_UNEXPECTED_NAME : exception;
  ERROR_PARAM : exception;
  ERROR_COUNT_PARAMS : exception;
  ERROR_ALREADY_DEFINED : exception;
  ERROR_NONDEFAULT_AFTER_DEFAULT : exception;

  function get return character;
  procedure define (name, params : string);
  procedure call (name, params : string);

private
  -- use type environment_t.cursor;

  end_macros_statement : constant string := ".end_macro";

  type param_t is record
    name : unb.unbounded_string;
    default_val : word := 0;
    has_default : boolean := false;
  end record;

  package params_t is new ada.containers.vectors(element_type => environment_t.cursor, index_type => natural, "=" => environment_t."=");
  package param_names_t is new ada.containers.indefinite_vectors(element_type => param_t, index_type => natural);

  type macros_t is record
    code : unb.unbounded_string;
    param_names : param_names_t.vector;
    num_required_params : natural := 0;
    first_line : pos_count;
  end record;

  current_macros : macros_t;

  package macroses_t is new ada.containers.indefinite_ordered_maps(element_type => macros_t, key_type => string);
  macroses : macroses_t.map;

  type called_macros_t is record
    macros_cursor : macroses_t.cursor;
    params : params_t.vector;
    last : positive := 1;
    cur_line : pos_count := 1;
  end record;

  current_called : called_macros_t;

  package stack_t is new ada.containers.vectors(element_type => called_macros_t, index_type => natural);
  stack : stack_t.vector;
end getter.macros;