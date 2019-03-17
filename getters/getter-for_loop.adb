package body getter.for_loop is
  function get return character is
    use type pos_count;

    c : character;
  begin
    if cur_for_loop.last > unb.length(cur_for_loop.code) then

      cur_for_loop.cur_line := cur_for_loop.first_line;
      cur_for_loop.last := 1;

      inc(cur_for_loop.cur_i);
      if cur_for_loop.cur_i > cur_for_loop.end_i then
        pop;
        environment.delete(cur_for_loop.cursor);
        for_loops.delete_last;
        if not for_loops_t.is_empty(for_loops) then
          cur_for_loop := for_loops_t.last_element(for_loops);
        end if;
      else
        environment.replace_element(cur_for_loop.cursor, word(cur_for_loop.cur_i));
      end if;

      return ' ';
    end if;
    c := unb.element(cur_for_loop.code, cur_for_loop.last);
    if c = ascii.lf then
      cur_for_loop.cur_line := cur_for_loop.cur_line + 1;
      set_line(cur_for_loop.cur_line);
    end if;
    inc(cur_for_loop.last);
    return c;
  end get;

  procedure create (start_i, end_i : word; var : environment_t.cursor) is
    ofs : natural := 1;
    eqs_start : natural := 1;
    eqs_end : natural := 1;
    len_start : constant natural := for_statement'length;
    len_end : constant natural := end_for_statement'length;
    c : character;
  begin
    tmp_for_loop.code := unb.to_unbounded_string(" ");
    tmp_for_loop.first_line := get_line;
    tmp_for_loop.cur_line := tmp_for_loop.first_line;
    tmp_for_loop.cursor := var;
    tmp_for_loop.start_i := natural(start_i);
    tmp_for_loop.cur_i := tmp_for_loop.start_i;
    tmp_for_loop.end_i := natural(end_i);
    environment.replace_element(var, start_i);

    loop
      c := getter.get(false);
      if c = ascii.nul then
        raise ERROR_NO_CLOSED;
      end if;
      if for_statement(eqs_start) = c and then unb.element(tmp_for_loop.code, unb.length(tmp_for_loop.code) - eqs_start + 1) in ' '|ascii.lf then
        inc(eqs_start);
        if eqs_start > len_start then
          inc(ofs);
          eqs_start := 1;
        end if;
      else
        eqs_start := 1;
      end if;
      if end_for_statement(eqs_end) = c and then unb.element(tmp_for_loop.code, unb.length(tmp_for_loop.code) - eqs_end + 1) in ' '|ascii.lf then
        inc(eqs_end);
        if eqs_end > len_end then
          eqs_end := 1;
          dec(ofs);
          if ofs = 0 then
            unb.delete(tmp_for_loop.code, unb.length(tmp_for_loop.code) - 6, unb.length(tmp_for_loop.code));
            exit;
          end if;
        end if;
      else
        eqs_end := 1;
      end if;
      unb.append(tmp_for_loop.code, c);
    end loop;

    if not for_loops_t.is_empty(for_loops) then
      for_loops.replace_element(for_loops_t.last(for_loops), cur_for_loop);
    end if;
    for_loops.append(tmp_for_loop);

    cur_for_loop := tmp_for_loop;
    getter.push(get'access);
  end create;
end getter.for_loop;