with numbers; use numbers;
with strings; use strings;
with env; use env;

package getter.high is
  ERROR_NO_CLOSED : exception;
  ERROR_HIGH : exception;

  function get return character;
  procedure start (s : string);
private
  use type unb.unbounded_string;

  type types is (type_byte, type_word);
  last_index : natural := 0;

  function is_type (s : string) return boolean;
end getter.high;