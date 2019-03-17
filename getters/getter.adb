package body getter is
  procedure push (getter : getter_type) is
  begin
    getters_stack.append(getter);
    current_getter := getter;
  end push;

  procedure pop is
  begin
    getters_stack.delete_last;
    if not getters_stack_t.is_empty(getters_stack) then
      current_getter := getters_stack_t.last_element(getters_stack);
    end if;
  end pop;

  function get (lf2space : boolean := true) return character is
    c : character;
  begin
    if getters_stack_t.is_empty(getters_stack) then
      raise ERROR_NO_GETTERS;
    end if;
    c := current_getter.all;

    if c = ascii.nul then
      getters_stack.delete_last;
      if getters_stack_t.is_empty(getters_stack) then
        return ascii.nul;
      end if;
      current_getter := getters_stack_t.last_element(getters_stack);
      return get;
    end if;
    if lf2space and c = ascii.lf then
      return ' ';
    end if;
    return c;
  end get;

  procedure set_line (cnt : pos_count) is
  begin
    current_line := cnt;
  end set_line;

  function get_line return pos_count is
  begin
    return current_line;
  end get_line;
end getter;