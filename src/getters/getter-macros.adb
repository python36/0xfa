package body getter.macros is
  function get return character is
    c : character;

    procedure rem_var (cur : in params_t.cursor) is
    begin
      environment.delete(environment_t.key(params_t.element(cur)));
    end rem_var;
  begin
    if current_called.last > unb.length(current_macros.code) then
      params_t.iterate(current_called.params, rem_var'access);
      stack.delete_last;
      if not stack_t.is_empty(stack) then
        current_called := stack_t.element(stack_t.last(stack));
        current_macros := macroses_t.element(current_called.macros_cursor);
      end if;
      pop;
      return ' ';
    end if;
    c := unb.element(current_macros.code, current_called.last);
    inc(current_called.last);
    return c;
  end get;

  procedure define (name, params : string) is
    eqs : natural := 1;
    c : character;
    tmp_macros : macros_t;
    len_end : constant natural := end_macros_statement'length;
    start_pos : natural := 1;
    cur_pos : natural := 1;
  begin
    loop
      c := getter.get(false);
      if c = ascii.nul then
        raise ERROR_NO_CLOSED;
      end if;

      if end_macros_statement(eqs) = c and then unb.element(tmp_macros.code, unb.length(tmp_macros.code) - eqs + 1) in ' '|ascii.lf then
        inc(eqs);
        if eqs > len_end then
          unb.delete(tmp_macros.code, unb.length(tmp_macros.code) - (end_macros_statement'length - 2), unb.length(tmp_macros.code));
          exit;
        end if;
      else
        eqs := 1;
      end if;

      unb.append(tmp_macros.code, c);
    end loop;

    for i in params'range loop
      c := params(i);
      if c in ' '|ascii.lf then
        if start_pos /= cur_pos then
          if not validate_variable(params(start_pos..(cur_pos - 1))) then
            raise ERROR_PARAM with params(start_pos..(cur_pos - 1));
          end if;
          tmp_macros.param_names.append(params(start_pos..(cur_pos - 1)));
        end if;
        start_pos := cur_pos + 1;
      end if;
      inc(cur_pos);
    end loop;

    macroses.insert(name, tmp_macros);
  end define;

  procedure call (name, params : string) is
    t_i : positive := 1;
    param_i : param_names_t.cursor;
    tmp_env_cur : environment_t.cursor;
    tb : boolean;
  begin
    if not macroses_t.contains(macroses, name) then
      raise ERROR_NO_DEFINED;
    end if;

    if not stack_t.is_empty(stack) then
      stack.replace_element(stack_t.last(stack), current_called);
    end if;

    current_called.last := 1;
    current_called.cur_line := 1;
    current_called.params.clear;
    current_called.macros_cursor := macroses_t.find(macroses, name);
    current_macros := macroses_t.element(current_called.macros_cursor);
    stack.append(current_called);

    if fix.count(params, " ") /= natural(param_names_t.length(current_macros.param_names)) then
      raise ERROR_COUNT_PARAMS;
    end if;

    param_i := param_names_t.first(current_macros.param_names);
    for i in params'range loop
      if params(i) = ' ' then
        environment.insert(param_names_t.element(param_i), value(params(t_i..i)), tmp_env_cur, tb);
        if not tb then
          raise ERROR_ALREADY_DEFINED;
        end if;
        current_called.params.append(tmp_env_cur);
        param_names_t.next(param_i);
        t_i := i + 1;
      end if;
    end loop;

    getter.push(get'access);
  end call;
end getter.macros;