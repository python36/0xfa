with interfaces;
with ada.unchecked_conversion;
with ada.numerics.generic_elementary_functions;
with ada.text_io;
with ada.strings.fixed;

package numbers is
  function uint32_to_integer is
    new ada.unchecked_conversion(source => interfaces.unsigned_32, target => integer);

  subtype byte is interfaces.unsigned_8;
  subtype word is interfaces.unsigned_16;

  function integer_to_word is
    new ada.unchecked_conversion(source => integer, target => word);

  use type byte;
  use type word;

  function words_add (a, b : word) return word;
  function words_sub (a, b : word) return word;
  function words_mul (a, b : word) return word;
  function words_div (a, b : word) return word;
  function words_pow (a, b : word) return word;
  function words_mod (a, b : word) return word;
  function words_or (a, b : word) return word;
  function words_and (a, b : word) return word;
  function words_xor (a, b : word) return word;
  function words_sl (a, b : word) return word;
  function words_sr (a, b : word) return word;

  function sl (value : byte; amount : natural) return byte renames interfaces.shift_left;
  function sr (value : byte; amount : natural) return byte renames interfaces.shift_right;
  function sl (value : word; amount : natural) return word renames interfaces.shift_left;
  function sr (value : word; amount : natural) return word renames interfaces.shift_right;

  procedure inc (p_i : in out integer);
  procedure dec (p_i : in out integer);
  procedure inc (p_i : in out word);
  procedure dec (p_i : in out word);
  procedure inc (p_i : in out byte);
  procedure dec (p_i : in out byte);
  procedure incd (p_i : in out byte);
  procedure incd (p_i : in out word);
  function incd (p_i : word) return word;
  procedure decd (p_i : in out word);
  function swpb (w : word) return word;
  procedure swpb (w : in out word);

  function word_to_integer (w : word) return integer;

  function validate_word (str : string) return boolean;
  function value (str : string) return word;

  procedure void (w : word);

  function to_positive (b : boolean; v : positive := 1) return positive;
  function to_natural (b : boolean; v : natural := 1) return natural;
  function to_word (b : boolean; v : word := 1) return word;

  function image (w : word; base : positive; fix_size : boolean := true) return string;
  function hex (w : word; fix_size : boolean := true) return string;
  function bin (w : word; fix_size : boolean := true) return string;

  function odd (w : word) return boolean;
  function even (w : word) return boolean;

private
  package natural_io is new ada.text_io.integer_io(natural);
  package word_func is new ada.numerics.generic_elementary_functions(long_float);

  function get_max_image_length (base : positive; val : word := word'last) return positive;

end numbers;