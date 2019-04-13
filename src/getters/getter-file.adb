package body getter.file is
  procedure close is
  begin
    ada.text_io.close(current_ptr.all);
    free(current_ptr);
    files_stack.delete_last;
    if not files_stack_t.is_empty(files_stack) then
      current_ptr := files_stack_t.last_element(files_stack).descriptor;
      current_line := files_stack_t.last_element(files_stack).line;
      set_line(current_line);
    end if;
  end close;

  function get return character is
    use type pos_count;

    c : character;
  begin
    if ada.text_io.end_of_file(current_ptr.all) then
      if not sended_last_lf then
        sended_last_lf := true;
        -- return ' ';
        return ascii.lf;
      end if;
      sended_last_lf := false;
      close;
      return ascii.nul;
    end if;
    ada.text_io.get_immediate(current_ptr.all, c);
    if c = ';' then
      ada.text_io.get_immediate(current_ptr.all, c);
      while c /= ascii.lf loop
        if ada.text_io.end_of_file(current_ptr.all) then
          c := ' ';
          exit;
        end if;
        ada.text_io.get_immediate(current_ptr.all, c);
      end loop;
    end if;
    if c = ascii.lf then
      current_line := current_line + 1;
      set_line(current_line);
      -- return ' ';
    elsif c = ascii.nul then
      return ' ';
    end if;
    return c;
  end get;

  procedure open (path : string) is
  begin
    if not files_stack_t.is_empty(files_stack) then
      files_stack.replace_element(files_stack_t.last(files_stack), (current_ptr, current_line));
    end if;
    current_ptr := new ada.text_io.file_type;
    ada.text_io.open(current_ptr.all, ada.text_io.in_file, path);
    files_stack.append((current_ptr, 1));

    getter.push(get'access);
  end open;
end getter.file;