package body strings is
  procedure print (c : character) is
  begin
    print("" & c);
  end print;

  procedure print (us : unb.unbounded_string) is
  begin
    print(unb.to_string(us));
  end print;

  function endswith (s, p : string) return boolean is
  begin
    return s'length >= p'length and then s((s'last - p'length + 1)..s'last) = p;
  end endswith;

  function startswith (s, p : string) return boolean is
  begin
    return s'length >= p'length and then s(s'first..(s'first + p'length - 1)) = p;
  end startswith;

  function first_element (s : string) return character is
  begin
    if s /= "" then
      return s(s'first);
    end if;
    return ascii.nul;
  end first_element;

  function last_element (s : string) return character is
  begin
    if s /= "" then
      return s(s'last);
    end if;
    return ascii.nul;
  end last_element;

  function first_element (us : unb.unbounded_string) return character is
  begin
    if unb.length(us) > 0 then
      return unb.element(us, 1);
    end if;
    return ascii.nul;
  end first_element;

  function last_element (us : unb.unbounded_string) return character is
  begin
    if unb.length(us) > 0 then
      return unb.element(us, unb.length(us));
    end if;
    return ascii.nul;
  end last_element;

  function ltrim (s : string; c : character := ' ') return string is
  begin
    for i in s'range loop
      if s(i) /= c then
        return s(i..s'last);
      end if;
    end loop;
    return "";
  end ltrim;

  function rtrim (s : string; c : character := ' ') return string is
  begin
    for i in reverse s'range loop
      if s(i) /= c then
        return s(s'first..i);
      end if;
    end loop;
    return "";
  end rtrim;

  function trim (s : string; c : character := ' ') return string is
  begin
    return rtrim(ltrim(s, c), c);
  end trim;

  procedure clear (us : out unb.unbounded_string) is
  begin
    us := unb.null_unbounded_string;
  end clear;

  function remove_ret_car (s : string) return string is
  begin
    if s /= "" and then s(s'last) = ascii.cr then return s(s'first..(s'last - 1)); end if;
    return s;
  end remove_ret_car;

  function lowercase (s : string) return string is
  begin
    return ada.strings.fixed.translate(s, ada.strings.maps.constants.lower_case_map);
  end lowercase;

  function uppercase (s : string) return string is
  begin
    return ada.strings.fixed.translate(s, ada.strings.maps.constants.upper_case_map);
  end uppercase;

  function cdr (str : string; offset : positive := 1) return string is
  begin
    if str'last < str'first + offset then
      return "";
    end if;
    return str((str'first + offset)..str'last);
  end cdr;

  procedure void (s : string) is
  begin
    null;
  end void;

end strings;