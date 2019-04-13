with ada.text_io;
with ada.strings.fixed;
with ada.strings.maps.constants;
with ada.strings.unbounded;

with numbers; use numbers;

package strings is
  package natural_io is new ada.text_io.integer_io(natural);
  package unb renames ada.strings.unbounded;
  package fix renames ada.strings.fixed;

  procedure print (s : string) renames ada.text_io.put_line;
  procedure put (s : string) renames ada.text_io.put;
  procedure put (s : character) renames ada.text_io.put;

  procedure print (c : character);
  procedure print (us : unb.unbounded_string);

  function first_element (s : string) return character;
  function last_element (s : string) return character;
  function first_element (us : unb.unbounded_string) return character;
  function last_element (us : unb.unbounded_string) return character;

  function endswith (s, p : string) return boolean;
  function endswith (s : string; c : character) return boolean;
  function startswith (s, p : string) return boolean;
  function startswith (s : string; c : character) return boolean;

  function ltrim (s : string; c : character := ' ') return string;
  function rtrim (s : string; c : character := ' ') return string;
  function trim (s : string; c : character := ' ') return string;

  procedure clear (us : out unb.unbounded_string);

  function remove_ret_car (s : string) return string;
  function lowercase (s : string) return string;
  function uppercase (s : string) return string;

  function car (s : string) return character renames first_element;
  function cdr (str : string; offset : positive := 1) return string;

  procedure void (s : string);

  function "=" (us : unb.unbounded_string; s : string) return boolean;
  function "=" (s : string; us : unb.unbounded_string) return boolean;

end strings;