with ada.containers.indefinite_ordered_maps;
with gnat.regexp;

with numbers; use numbers;

package env is
  use type word;

  package environment_t is new ada.containers.indefinite_ordered_maps(
    element_type => word, key_type => string);
  environment : environment_t.map;

  function validate_variable (s : string) return boolean;

private
  variable_regexp : gnat.regexp.regexp := gnat.regexp.compile("[a-z][a-z0-9_]*");
end env;