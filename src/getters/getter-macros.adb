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
    mustbe_only_default : boolean := false;
    t_param : param_t;
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

          declare
            param : constant string := params(start_pos..(cur_pos - 1));
            assignment_pos : natural := fix.index(param, "=");
          begin
            if assignment_pos > 0 then
              mustbe_only_default := true;
              if assignment_pos = param'last or assignment_pos = param'first or
                  not validate_variable(param(param'first..(assignment_pos - 1))) or
                  not validate_word(param((assignment_pos + 1)..param'last)) then
                raise ERROR_PARAM with param;
              end if;
              t_param.name := unb.to_unbounded_string(param(param'first..(assignment_pos - 1)));
              t_param.default_val := value(param((assignment_pos + 1)..param'last));
              t_param.has_default := true;
            elsif mustbe_only_default then
              raise ERROR_NONDEFAULT_AFTER_DEFAULT with param;
            elsif not validate_variable(param) then
              raise ERROR_PARAM with param;
            else
              inc(tmp_macros.num_required_params);
              t_param.name := unb.to_unbounded_string(param);
            end if;
          end;

          tmp_macros.param_names.append(t_param);
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
    num_req : natural;
    tmp_n : natural;
    mustbe_only_default : boolean := false;
    t_param : param_t;
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
    num_req := current_macros.num_required_params;
    stack.append(current_called);

    param_i := param_names_t.first(current_macros.param_names);
    for i in params'range loop
      if params(i) = ' ' then
        declare
          param_raw : constant string := params(t_i..i-1);
          assignment_pos : natural := fix.index(param_raw, "=");
          name : constant string := param_raw(param_raw'first..(assignment_pos - 1));
          param : constant string := param_raw(natural'max((assignment_pos + 1), param_raw'first)..param_raw'last);
        begin
          if assignment_pos > 0 then
            if assignment_pos = param_raw'first or assignment_pos = param_raw'last then
              raise ERROR_PARAM with param;
            end if;
            mustbe_only_default := true;

            param_i := param_names_t.first(current_macros.param_names);
            while param_names_t.has_element(param_i) loop
              t_param := param_names_t.element(param_i);
              if t_param.name = name then
                if not t_param.has_default then
                  if num_req = 0 then
                    raise ERROR_COUNT_PARAMS with params;
                  end if;
                  dec(num_req);
                end if;
                exit;
              end if;
              param_names_t.next(param_i);
            end loop;
            if not param_names_t.has_element(param_i) then
              raise ERROR_UNEXPECTED_NAME with name;
            end if;
            environment.insert(name, value(param), tmp_env_cur, tb);
          elsif mustbe_only_default then
            raise ERROR_NONDEFAULT_AFTER_DEFAULT with params;
          else
            if num_req > 0 then
              dec(num_req);
            end if;
            environment.insert(unb.to_string(param_names_t.element(param_i).name), value(param), tmp_env_cur, tb);
            param_names_t.next(param_i);
          end if;

          if not tb then
            raise ERROR_ALREADY_DEFINED;
          end if;
        end;

        current_called.params.append(tmp_env_cur);
        t_i := i + 1;
      end if;
    end loop;

    if num_req > 0 then
      raise ERROR_COUNT_PARAMS with params;
    end if;

    param_i := param_names_t.first(current_macros.param_names);
    while param_names_t.has_element(param_i) loop
      t_param := param_names_t.element(param_i);
      if t_param.has_default then
        environment.insert(unb.to_string(param_names_t.element(param_i).name), t_param.default_val, tmp_env_cur, tb);
        if tb then
          current_called.params.append(tmp_env_cur);
        end if;
      end if;
      param_names_t.next(param_i);
    end loop;

    getter.push(get'access);
  end call;
end getter.macros;
