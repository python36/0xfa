package body numbers is

  procedure inc (p_i : in out integer) is begin p_i := p_i + 1; end inc;
  procedure dec (p_i : in out integer) is begin p_i := p_i - 1; end dec;
  procedure inc (p_i : in out word) is begin p_i := p_i + 1; end inc;
  procedure dec (p_i : in out word) is begin p_i := p_i - 1; end dec;
  procedure inc (p_i : in out byte) is begin p_i := p_i + 1; end inc;
  procedure dec (p_i : in out byte) is begin p_i := p_i - 1; end dec;
  procedure incd (p_i : in out byte) is begin p_i := p_i + 2; end incd;
  procedure incd (p_i : in out word) is begin p_i := p_i + 2; end incd;
  function incd (p_i : word) return word is begin return p_i + 2; end incd;
  procedure decd (p_i : in out word) is begin p_i := p_i - 2; end decd;
  function swpb (w : word) return word is begin return sr(w, 8) or sl(w, 8); end swpb;
  procedure swpb (w : in out word) is begin w := swpb(w); end swpb;

  function words_add (a, b : word) return word is begin return a + b; end words_add;
  function words_sub (a, b : word) return word  is begin return a - b; end words_sub;
  function words_mul (a, b : word) return word  is begin return a * b; end words_mul;
  function words_div (a, b : word) return word  is begin return a / b; end words_div;
  function words_pow (a, b : word) return word  is begin return a ** natural(b); end words_pow;
  function words_mod (a, b : word) return word  is begin return a mod b; end words_mod;
  function words_or (a, b : word) return word  is begin return a or b; end words_or;
  function words_and (a, b : word) return word  is begin return a and b; end words_and;
  function words_xor (a, b : word) return word  is begin return a xor b; end words_xor;
  function words_sl (a, b : word) return word  is begin return sl(a, natural(b)); end words_sl;
  function words_sr (a, b : word) return word  is begin return sr(a, natural(b)); end words_sr;

  function word_to_integer (w : word) return integer is
    use type interfaces.unsigned_32;
  begin
    return uint32_to_integer(
      (4294934528 * interfaces.unsigned_32(sr(w, 15))) or interfaces.unsigned_32(w));
  end word_to_integer;

  function validate_word (str : string) return boolean is
  begin
    void(value(str));
    return true;
  exception
    when constraint_error => return false;
  end validate_word;

  function value (str : string) return word is
    t_i : integer;
  begin
    if str(str'first) = '-' then
      t_i := integer'value(str);
      if t_i < -32768 then
        raise constraint_error;
      end if;
      return integer_to_word(t_i);
    elsif str'length > 2 and then str(str'first..(str'first + 1)) = "0x" then
      return word'value("16#" & str((str'first + 2)..str'last) & '#');
    else
      return word'value(str);
    end if;
  end value;

  procedure void (w : word) is
  begin
    null;
  end void;

  function to_positive (b : boolean; v : positive := 1) return positive is
  begin
    if b then
      return v;
    end if;
    return positive'first;
  end to_positive;

  function to_natural (b : boolean; v : natural := 1) return natural is
  begin
    if b then
      return v;
    end if;
    return natural'first;
  end to_natural;

  function to_word (b : boolean; v : word := 1) return word is
  begin
    if b then
      return v;
    end if;
    return word'first;
  end to_word;

  function get_max_image_length (base : positive; val : word := word'last) return positive is
  begin
    if val = 0 then
      return 1;
    end if;
    return positive(long_float'floor(word_func.log(x => long_float(val), base => long_float(base))) + 1.0);
  end get_max_image_length;

  function image (w : word; base : positive; fix_size : boolean := true) return string is
    pragma assertion_policy (CHECK);
    pragma assert (
      base in ada.text_io.number_base'range,
      string'("base " & base'img & " not in range " & ada.text_io.number_base'first'img & ".." & ada.text_io.number_base'last'img));
    base_img_len : constant positive := base'img'length;
    str : string(1..get_max_image_length(base, w) + base_img_len + 1) := (others => '*');
  begin
    natural_io.put(str, natural(w), base);
    if fix_size then
      return ada.strings.fixed.tail(str((str'first + base_img_len)..(str'last - 1)), get_max_image_length(base, word'last), '0');
    end if;
    return str((str'first + base_img_len)..(str'last - 1));
  end image;

  function hex (w : word; fix_size : boolean := true) return string is
  begin
    return "0x" & image(w, 16, fix_size);
  end hex;

  function bin (w : word; fix_size : boolean := true) return string is
  begin
    return "0b" & image(w, 2, fix_size);
  end bin;

  function odd (w : word) return boolean is
  begin
    return (w and 1) = 1;
  end odd;

  function even (w : word) return boolean is
  begin
    return (w and 1) = 0;
  end even;

end numbers;