package body getter.high is
  function get return character is
    c : character;
  begin
    pop;
    return ' ';
  end get;

  function get_word (s : string; n : natural := 0; force : boolean := false) return string is
    c : natural := 0;
    ns : string := s & ' ';
    i, j : natural;
  begin
    if force then
      last_index := 0;
      i := ns'first;
    else
      i := natural'max(last_index, ns'first);
    end if;
    j := i;
    for char of ns(i..ns'last) loop
      if char = ' ' then
        if j /= i then
          if n = c then
            exit;
          end if;
          inc(c);
        end if;
        j := i + 1;
      end if;
      inc(i);
    end loop;
    last_index := i + 1;
    return s(j..(i - 1));
  end get_word;

  procedure process (s : string) is
    first_word : string := get_word(s, 0, True);
    tus : unb.unbounded_string;
    ti : natural;
  begin
    if first_word = "" then
      return;
    end if;
    if is_type(first_word) then
      loop
        tus := unb.to_unbounded_string(get_word(s));
        if tus = unb.null_unbounded_string then
          exit;
        end if;
        ti := unb.index(tus, ":");
        if ti = 1 then
          raise ERROR_HIGH;
        elsif ti = 0 then
          ti := unb.length(tus);
        else
          if not validate_word(unb.slice(tus, ti + 1, unb.length(tus))) then
            raise ERROR_HIGH;
          end if;
          ti := ti - 1;
        end if;
        -- print(unb.slice(tus, 1, ti));
        if not validate_variable(unb.slice(tus, 1, ti)) then
          raise ERROR_HIGH;
        end if;
      end loop;
      print("T");
    end if;
    print(get_word(s));
    print("-- " & first_word);
  end process;

  procedure start (s : string) is
    ignore_end : boolean := false;
    c : character;
    us : unb.unbounded_string;
  begin
    loop
      c := getter.get(false);
      if c = ascii.nul then
        raise ERROR_NO_CLOSED;
      end if;
      if c = '`' and not ignore_end then
        exit;
      end if;
      if c = ascii.lf then
        process(trim(unb.to_string(us)));
        us := unb.null_unbounded_string;
      else
        unb.append(us, c);
      end if;
    end loop;
    getter.push(get'access);
  end start;

  function is_type (s : string) return boolean is
    t : types;
  begin
    t := types'value("type_" & s);
    return true;
  exception
    when others => return false;
  end is_type;
end getter.high;