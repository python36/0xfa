with ada.text_io;
with ada.command_line;
with gnat.os_lib;
with ada.strings;
with ada.strings.bounded;
with ada.containers.vectors;
with ada.containers.indefinite_ordered_maps;
with ada.containers.ordered_maps;
with ada.containers.doubly_linked_lists;
with interfaces;
with ada.exceptions;
with ada.containers;
with ada.io_exceptions;
with ada.finalization;

with getter;
with getter.file;
with getter.for_loop;
with getter.macros;

with numbers; use numbers;
with strings; use strings;
with address; use address;
with labels; use labels;
with env; use env;

procedure a0xfa is
  use type byte;
  use type word;

  parse_error : exception;

  programm_address : pos_addr_t := create(0);
  ff : constant word := 16#ffff#;

  r_pc : constant word := 0;
  r_sp : constant word := 1;
  r_sr : constant word := 2;

  procedure error (s : string) is
  begin
    print(s);
    gnat.os_lib.os_exit(1);
  end error;

  procedure error_parse (s : string) is
  begin
    raise parse_error with s;
  end error_parse;

  procedure warning (msg : string) is
  begin
    print("[warning]: " & msg);
  end warning;

  type record_handler_type;
  type handler_type is access function (s : string) return record_handler_type;
  type record_handler_type is record
    ptr : handler_type;
  end record;

  type operation_access is access function (a, b : word) return word;

  parse_def_cursor : environment_t.cursor := environment_t.no_element;

  type operation_t is record
    operation_ptr : operation_access := words_add'access;
    priority : positive := 1;
    operand_pos : natural;
  end record;

  package operations_t is new ada.containers.vectors(
    element_type => operation_t, index_type => natural);

  package operands_t is new ada.containers.vectors(
    element_type => word, index_type => natural);

  function ">" (a, b : operation_t) return boolean is
  begin
    return a.priority > b.priority;
  end;

  procedure insert_sorted (ops : in out operations_t.vector; op : operation_t) is
    tmp_cursor : operations_t.cursor := ops.first;
  begin
    while operations_t.has_element(tmp_cursor) loop
      if op > operations_t.element(tmp_cursor) then
        operations_t.insert(ops, tmp_cursor, op);
        return;
      end if;
      operations_t.next(tmp_cursor);
    end loop;
    operations_t.append(ops, op);
  end insert_sorted;

  procedure environment_repr is
    tmp_cursor : environment_t.cursor := environment.first;
  begin
    while environment_t.has_element(tmp_cursor) loop
      print(environment_t.key(tmp_cursor) & " : " & environment_t.element(tmp_cursor)'img);
      environment_t.next(tmp_cursor);
    end loop;
  end environment_repr;

  type commands is (
    f_rrc, f_swpb, f_rra, f_sxt, f_push, f_call, f_reti,
    f_jne, f_jeq, f_jnc, f_jc, f_jn, f_jge, f_jl, f_jmp,
    f_mov, f_add, f_addc, f_subc, f_sub, f_cmp, f_dadd,
      f_bit, f_bic, f_bis, f_xor, f_and,
    f_jnz, f_jz, f_jlo, f_jhs,
    f_clrc, f_clrn, f_clrz, f_dint, f_eint, f_nop, f_ret, f_setc, f_setn, f_setz,
    f_adc, f_br, f_clr, f_dadc, f_dec, f_decd, f_inc,
      f_incd, f_inv, f_pop, f_rla, f_rlc, f_sbc, f_tst,
    f_raw);
  subtype single_commands is commands range f_rrc..f_reti;
  subtype jump_commands is commands range f_jne..f_jmp;
  subtype alternatives_jump_commands is commands range f_jnz..f_jhs;
  subtype two_commands is commands range f_mov..f_and;
  subtype emulated_commands is commands range f_jnz..f_tst;
  subtype zero_emulated_commands is commands range f_clrc..f_setz;
  subtype single_emulated_commands is commands range f_adc..f_tst;

  type addressing_modes is (
    m_constant,
    m_immediate, m_indexed, m_symbolic, m_absolute,
    m_indirect_register, m_indirect_autoincrement, m_register);

  type operand_t is record
    mode : addressing_modes := m_constant;
    value : word := 0;
    register : word := 0;
    label : label_t := null_label;
  end record;
  null_operand : constant operand_t := (m_register, 0, ff, null_label);

  type command_t is record
    command : commands;
    source, destination : operand_t := null_operand;
    num_operands : byte := 0;
    bw : boolean := true;
  end record;
  package programm_t is new ada.containers.ordered_maps(
    element_type => command_t, key_type => pos_addr_t);
  programm : programm_t.map;
  null_command : command_t := (f_raw, null_operand, null_operand, 0, true);
  type words_t is array (word range <>) of word;

  command : command_t := null_command;

  procedure void (r : record_handler_type) is begin null; end void;

  function parse_main (s : string) return record_handler_type;
  -- procedure from_file (path : string);

  function parse_word (s : string) return word is
  begin
    return value(s);
  exception
    when constraint_error => error_parse("bad value " & s); return 0;
  end parse_word;

  function validate_register (s : string) return boolean is
  begin
    return car(s) = 'r' and then natural'value(cdr(s)) in 0..15;
  exception
    when constraint_error => return false;
  end validate_register;

  procedure validate_variable_assert (s : string) is
  begin
    if not validate_variable(s) then
      error_parse("error var name " & s);
    end if;
  end validate_variable_assert;

  function parse_def (s : string) return record_handler_type is
    use type environment_t.cursor;
    tb : boolean;
  begin
    if parse_def_cursor = environment_t.no_element then
      validate_variable_assert(s);
      environment.insert(s, 0, parse_def_cursor, tb);
      if not tb then
        warning("variable """ & s & """ redefined");
      end if;
      return (ptr => parse_def'access);
    end if;
    environment.replace_element(parse_def_cursor, parse_word(s));
    parse_def_cursor := environment_t.no_element;
    return (ptr => parse_main'access);
  end parse_def;

  function parse_defs (s : string) return record_handler_type is
  begin
    if s = ".end_defs" then
      return (ptr => parse_main'access);
    end if;
    void(parse_def(s));
    return (ptr => parse_defs'access);
  end parse_defs;

  function parse_undef (s : string) return record_handler_type is
  begin
    validate_variable_assert(s);
    if not environment_t.contains(environment, s) then
      warning("variable name """ & s & """ not defined");
    else
      environment.delete(s);
    end if;
    return (ptr => parse_main'access);
  end parse_undef;

  function parse_set (s : string) return record_handler_type is
  begin
    validate_variable_assert(s);
    if not environment_t.contains(environment, s) then
      error_parse("variable name """ & s & """ not defined");
    end if;
    void(parse_undef(s));
    return parse_def(s);
  end parse_set;

  function parse_include (s : string) return record_handler_type is
  begin
    if first_element(s) /= '"' or last_element(s) /= '"' then
      error_parse("error .include format");
    end if;
    getter.file.open(trim(s((s'first + 1)..(s'last - 1))));
    return (ptr => parse_main'access);
  end parse_include;

  type for_loop_step_t is (for_loop_var, for_loop_mode, for_loop_range);
  for_loop_step : for_loop_step_t := for_loop_var;

  tmp_for_loop_cursor : environment_t.cursor;

  function parse_for_loop (s : string) return record_handler_type is
    tb : boolean;
    dots_pos : natural;
  begin
    if for_loop_step = for_loop_var then
      validate_variable_assert(s);
      environment.insert(s, 0, tmp_for_loop_cursor, tb);
      if not tb then
        error_parse("variable name """ & s & """ already defined");
      end if;
      for_loop_step := for_loop_mode;
    elsif for_loop_step = for_loop_mode then
      if s /= "in" then
        error_parse("error format for: " & s);
      end if;
      for_loop_step := for_loop_range;
    elsif for_loop_step = for_loop_range then
      dots_pos := fix.index(s, "..");
      if dots_pos < 2 or (dots_pos + 1) >= s'last then
        error_parse("error range: " & s);
      end if;
      getter.for_loop.create(
        parse_word(s(s'first..(dots_pos - 1))),
        parse_word(s((dots_pos + 2)..s'last)),
        tmp_for_loop_cursor);
      for_loop_step := for_loop_var;
      return (ptr => parse_main'access);
    end if;
    return (ptr => parse_for_loop'access);
  end parse_for_loop;

  macro_name : unb.unbounded_string := unb.null_unbounded_string;
  macro_params : unb.unbounded_string := unb.null_unbounded_string;
  macro_wait_open : boolean := false;

  function get_macros_name_params (s : string) return boolean is
    use type unb.unbounded_string;

    open_pos : natural := 1;

    procedure set_macro_name (name : string) is
    begin
      if not validate_variable(name) then
        error_parse("error macros name: " & name);
      end if;
      macro_name := unb.to_unbounded_string(name);
    end set_macro_name;

    procedure append_param (s : string) is
    begin
      if s'length = 0 then
        return;
      end if;
      unb.append(macro_params, s & ' ');
    end append_param;
  begin
    if macro_name = unb.null_unbounded_string then
      macro_wait_open := true;
      open_pos := fix.index(s, "(");
      if open_pos > 0 then
        set_macro_name(s(s'first..(open_pos - 1)));
      else
        set_macro_name(s);
        return false;
      end if;
    end if;

    if macro_wait_open then
      if s(open_pos) /= '(' then
        error_parse("wait '(' but: " & s);
      end if;
      macro_wait_open := false;
      inc(open_pos);
    end if;

    if endswith(s, ')') then
      if s'length > 1 then
        append_param(s(open_pos..(s'last - 1)));
      end if;
      return true;
    else
      append_param(s(open_pos..s'last));
    end if;

    return false;
  end get_macros_name_params;

  function parse_macros (s : string) return record_handler_type is
  begin
    if get_macros_name_params(s) then
      getter.macros.define(unb.to_string(macro_name), unb.to_string(macro_params));
      macro_name := unb.null_unbounded_string;
      macro_params := unb.null_unbounded_string;
      return (ptr => parse_main'access);
    end if;
    return (ptr => parse_macros'access);
  end parse_macros;

  function parse_instruction (s : string) return record_handler_type is
  begin
    if s = ".def" then
      return (ptr => parse_def'access);
    elsif s = ".undef" then
      return (ptr => parse_undef'access);
    elsif s = ".set" then
      return (ptr => parse_set'access);
    elsif s = ".include" then
      return (ptr => parse_include'access);
    elsif s = ".for" then
      return (ptr => parse_for_loop'access);
    elsif s = ".macro" then
      return (ptr => parse_macros'access);
    elsif s = ".defs" then
      return (ptr => parse_defs'access);
    end if;
    error_parse("unknown instruction " & s);
    return (ptr => parse_main'access);
  end parse_instruction;

  function in_labels (s : string) return boolean is
  begin
    return get_by_name(s) /= null_label;
  end in_labels;

  function validate_command (s : string) return boolean is
  begin
    return commands'value("f_" &
      s(s'first..(s'last - to_natural(endswith(s, ".b"), 2) - to_natural(endswith(s, ".w"), 2)))) >= commands'first;
  exception
    when others => return false;
  end validate_command;

  function parse_label (s : string) return record_handler_type is
    tmp_word : word;
    label : label_t;
  begin
    if first_element(s) in '0'..'9' then
      tmp_word := parse_word(s);
      if not valid_value_for_pos_addr(tmp_word) then
        error_parse("bad value address " & s);
      end if;

      if last_label /= null_label and then programm_address = get(last_label) then
        error_parse("set address must be before define label");
      end if;
      programm_address := create(tmp_word);
    elsif first_element(s) in 'a'..'z' then
      validate_variable_assert(s);
      if validate_register(s) then
        error_parse(s & " label can not be r0..15");
      elsif validate_command(s) then
        error_parse(s & " label can not be command");
      end if;
      label := get_by_name(s);
      if label /= null_label then
        if get(label) = null_pos_addr then
          label.set(programm_address);
        else
          error_parse("label " & s & " already defined");
        end if;
      else
        create(s, programm_address);
      end if;
    else
      error_parse("invalid data " & s);
    end if;
    return (ptr => parse_main'access);
  end parse_label;

  function get_var_val (s : string) return word is
    tmp_cursor : environment_t.cursor;
  begin
    tmp_cursor := environment_t.find(environment, s);
    -- environment_repr;
    if environment_t.has_element(tmp_cursor) then
      return environment_t.element(tmp_cursor);
    end if;
    error_parse("variable " & s & " undefined");
    return 0;
  end get_var_val;

  function parse_precompile (s : string) return string is
    use type ada.containers.count_type;

    start_pos, end_pos : natural := 0;
    value : word := 0;
    operation : operation_access := words_add'access;
    i : natural := s'first;
    priority : positive;
    operations : operations_t.vector;
    operands : operands_t.vector;
    tmp_operation : operation_t;
    k_priority : natural := 0;
  begin
    while i < s'last + 1 loop
      if s(i) in '0'..'9'|'a'..'z'|'_' then
        if start_pos = end_pos then
          start_pos := i;
          end_pos := i + 1;
        else
          if i /= end_pos then
            error_parse("error " & s(start_pos..i));
          end if;
          inc(end_pos);
        end if;
      else
        if s(i) = ' ' then
          null;
        elsif s(i) = '(' then
          k_priority := k_priority + 10;
        elsif s(i) = ')' then
          if k_priority = 0 then
            error_parse(s & " ')' > '('");
          end if;
          k_priority := k_priority - 10;
        elsif start_pos /= end_pos then
          if s(start_pos) in 'a'..'z' then
            operands.append(get_var_val(s(start_pos..(end_pos - 1))));
          else
            -- print(s(start_pos..(end_pos - 1)));
            operands.append(parse_word(s(start_pos..(end_pos - 1))));
          end if;

          if s(i) = ';' then
            exit;
          elsif s(i) = '<' then
            if s(i + 1) /= '<' then
              error_parse(s & " unknown operation. maybe << ?");
            end if;
            inc(i);
            operation := words_sl'access;
            priority := k_priority + 4;
          elsif s(i) = '>' then
            if s(i + 1) /= '>' then
              error_parse(s & " unknown operation. maybe >> ?");
            end if;
            inc(i);
            operation := words_sr'access;
            priority := k_priority + 4;
          elsif s(i) = '+' then
            operation := words_add'access;
            priority := k_priority + 5;
          elsif s(i) = '-' then
            operation := words_sub'access;
            priority := k_priority + 5;
          elsif s(i) = '*' then
            if s(i + 1) = '*' then
              operation := words_pow'access;
              priority := k_priority + 7;
              inc(i);
            else
              operation := words_mul'access;
              priority := k_priority + 6;
            end if;
          elsif s(i) = '/' then
            operation := words_div'access;
            priority := k_priority + 6;
          elsif s(i) = '%' then
            operation := words_mod'access;
            priority := k_priority + 6;
          elsif s(i) = '|' then
            operation := words_or'access;
            priority := k_priority + 1;
          elsif s(i) = '&' then
            operation := words_and'access;
            priority := k_priority + 3;
          elsif s(i) = '^' then
            operation := words_xor'access;
            priority := k_priority + 2;
          else
            error_parse(s & " unknown operation");
          end if;
          insert_sorted(operations, (operation, priority, operands.last_index));
          start_pos := end_pos;
        else
          error_parse(s & " error");
        end if;
      end if;
      inc(i);
    end loop;
    if k_priority /= 0 then
      error_parse(s & " '(' > ')'");
    end if;

    if operations_t.length(operations) = 0 and operands_t.length(operands) = 1 then
      return ltrim(operands_t.first_element(operands)'img);
    end if;
    while operations_t.length(operations) > 0 loop
      tmp_operation := operations_t.first_element(operations);
      value := tmp_operation.operation_ptr.all(
        operands_t.element(operands, tmp_operation.operand_pos),
        operands_t.element(operands, tmp_operation.operand_pos + 1));
      operands_t.replace_element(operands, tmp_operation.operand_pos, value);
      operands_t.replace_element(operands, tmp_operation.operand_pos + 1, value);
      operations_t.delete_first(operations);
    end loop;

    return ltrim(value'img);
  end parse_precompile;

  function operand_has_ext_word (operand : operand_t) return boolean is
  begin
    return operand.mode in m_symbolic|m_absolute|m_immediate|m_indexed;
  end operand_has_ext_word;

  procedure command_to_programm (cmd : in out command_t) is
    tmp_cursor : programm_t.cursor;
    inserted : boolean;
    prev_addr : pos_addr_t;
    prev_cursor : programm_t.cursor;

    procedure inc_addr_by_mode (operand : operand_t; addr : in out pos_addr_t) is
    begin
      if operand /= null_operand then
        if operand_has_ext_word(operand) then
          inc(addr);
        end if;
      end if;
    end inc_addr_by_mode;
  begin
    if cmd.command in emulated_commands then
      case cmd.command is
        when f_adc =>
          cmd.command := f_addc;
          cmd.destination := cmd.source;
          cmd.source.mode := m_constant;
          cmd.source.value := 0;
        when f_br =>
          cmd.command := f_mov;
          cmd.destination.mode := m_register;
          cmd.destination.register := r_pc;
        when f_clr =>
          cmd.command := f_mov;
          cmd.destination := cmd.source;
          cmd.source.mode := m_constant;
          cmd.source.value := 0;
        when f_clrc =>
          cmd.command := f_bic;
          cmd.source.mode := m_constant;
          cmd.source.value := 1;
          cmd.destination.mode := m_register;
          cmd.destination.register := r_sr;
        when f_clrn =>
          cmd.command := f_bic;
          cmd.source.mode := m_constant;
          cmd.source.value := 4;
          cmd.destination.mode := m_register;
          cmd.destination.register := r_sr;
        when f_clrz =>
          cmd.command := f_bic;
          cmd.source.mode := m_constant;
          cmd.source.value := 2;
          cmd.destination.mode := m_register;
          cmd.destination.register := r_sr;
        when f_dadc =>
          cmd.command := f_dadd;
          cmd.destination := cmd.source;
          cmd.source.mode := m_constant;
          cmd.source.value := 0;
        when f_dec =>
          cmd.command := f_sub;
          cmd.destination := cmd.source;
          cmd.source.mode := m_constant;
          cmd.source.value := 1;
        when f_decd =>
          cmd.command := f_sub;
          cmd.destination := cmd.source;
          cmd.source.mode := m_constant;
          cmd.source.value := 2;
        when f_dint =>
          cmd.command := f_bic;
          cmd.source.mode := m_constant;
          cmd.source.value := 8;
          cmd.destination.mode := m_register;
          cmd.destination.register := r_sr;
        when f_eint =>
          cmd.command := f_bis;
          cmd.source.mode := m_constant;
          cmd.source.value := 8;
          cmd.destination.mode := m_register;
          cmd.destination.register := r_sr;
        when f_inc =>
          cmd.command := f_add;
          cmd.destination := cmd.source;
          cmd.source.mode := m_constant;
          cmd.source.value := 1;
        when f_incd =>
          cmd.command := f_add;
          cmd.destination := cmd.source;
          cmd.source.mode := m_constant;
          cmd.source.value := 2;
        when f_inv =>
          cmd.command := f_xor;
          cmd.destination := cmd.source;
          cmd.source.mode := m_constant;
          cmd.source.value := 16#ffff#;
        when f_nop =>
          cmd.command := f_mov;
          cmd.source.mode := m_constant;
          cmd.source.value := 0;
          cmd.destination.mode := m_register;
          cmd.destination.register := 3;
        when f_pop =>
          cmd.command := f_mov;
          cmd.destination := cmd.source;
          cmd.source.mode := m_indirect_autoincrement;
          cmd.source.register := r_sp;
        when f_ret =>
          cmd.command := f_mov;
          cmd.source.mode := m_indirect_autoincrement;
          cmd.source.register := r_sp;
          cmd.destination.mode := m_register;
          cmd.destination.register := r_pc;
        when f_rla =>
          cmd.command := f_add;
          cmd.destination := cmd.source;
        when f_rlc =>
          cmd.command := f_addc;
          cmd.destination := cmd.source;
        when f_sbc =>
          cmd.command := f_subc;
          cmd.destination := cmd.source;
          cmd.source.mode := m_constant;
          cmd.source.value := 0;
        when f_setc =>
          cmd.command := f_bis;
          cmd.source.mode := m_constant;
          cmd.source.value := 1;
          cmd.destination.mode := m_register;
          cmd.destination.register := r_sr;
        when f_setn =>
          cmd.command := f_bis;
          cmd.source.mode := m_constant;
          cmd.source.value := 4;
          cmd.destination.mode := m_register;
          cmd.destination.register := r_sr;
        when f_setz =>
          cmd.command := f_bis;
          cmd.source.mode := m_constant;
          cmd.source.value := 2;
          cmd.destination.mode := m_register;
          cmd.destination.register := r_sr;
        when f_tst =>
          cmd.command := f_cmp;
          cmd.destination := cmd.source;
          cmd.source.mode := m_constant;
          cmd.source.value := 0;
        when f_jnz =>
          cmd.command := f_jne;
        when f_jz =>
          cmd.command := f_jeq;
        when f_jlo =>
          cmd.command := f_jnc;
        when f_jhs =>
          cmd.command := f_jc;
        when others => null;
      end case;
    elsif cmd.command = f_reti then
      cmd.source.mode := m_register;
      cmd.source.register := 0;
    end if;

    programm_t.insert(programm, programm_address, cmd, tmp_cursor, inserted);
    if not inserted then
      error_parse("rewrite command: " & hex(get(programm_address)));
    end if;
    prev_cursor := programm_t.previous(tmp_cursor);
    if programm_t.has_element(prev_cursor) then
      prev_addr := programm_t.key(prev_cursor);
      if programm_t.element(prev_cursor).command not in jump_commands|alternatives_jump_commands|f_raw then
        inc_addr_by_mode(programm_t.element(prev_cursor).source, prev_addr);
        inc_addr_by_mode(programm_t.element(prev_cursor).destination, prev_addr);
      end if;
      if prev_addr >= programm_address and programm_t.key(prev_cursor) < programm_address then
        error_parse("rewrite previous command: " & hex(get(programm_address)));
      end if;
    end if;

    inc(programm_address);
    if cmd.command not in jump_commands|alternatives_jump_commands|f_raw then
      inc_addr_by_mode(cmd.source, programm_address);
      inc_addr_by_mode(cmd.destination, programm_address);
    end if;
    tmp_cursor := programm_t.next(tmp_cursor);
    if programm_t.has_element(tmp_cursor) then
      if programm_t.key(tmp_cursor) <= programm_address then
        error_parse("rewrite next command: " & hex(get(programm_address)));
      end if;
    end if;
  end command_to_programm;

  function parse_register (s : string) return word is
  begin
    if not validate_register(s) then
      error_parse(s & " is not register");
    end if;
    return word'value(cdr(s));
  end parse_register;

  procedure parse_operand (addr : in out operand_t; s : string) is
    open_pos, close_pos : natural;
  begin
    if first_element(s) = '#' then
      addr.mode := m_immediate;
      if s(s'first + 1) in '0'..'9'|'-' then
        addr.value := parse_word(cdr(s));
        if addr.value in 0..2|4|8|ff then
          addr.mode := m_constant;
        end if;
      elsif in_labels(cdr(s)) then
        addr.label := get_by_name(cdr(s));
      else
        addr.label := create(cdr(s));
      end if;
    elsif first_element(s) = ''' and s'length = 3 then
      addr.mode := m_immediate;
      addr.value := character'pos(s(s'first + 1));
    elsif first_element(s) = '@' then
      if last_element(s) = '+' then
        addr.mode := m_indirect_autoincrement;
        addr.register := parse_register(s((s'first + 1)..(s'last - 1)));
      else
        addr.mode := m_indirect_register;
        addr.register := parse_register(cdr(s));
      end if;
    elsif validate_register(s) then
      addr.mode := m_register;
      addr.register := parse_register(s);
    elsif first_element(s) = '&' and then s'length > 1 then
      addr.mode := m_absolute;
      if s(s'first + 1) in '0'..'9'|'-' then
        addr.value := parse_word(cdr(s));
        addr.label := null_label;
      elsif in_labels(cdr(s)) then
        addr.label := get_by_name(cdr(s));
      else
        addr.label := create(cdr(s));
      end if;
    elsif first_element(s) in '0'..'9' and last_element(s) /= ')' then
      addr.mode := m_symbolic;
      addr.value := parse_word(s);
      addr.label := null_label;
    elsif in_labels(s) then
      addr.mode := m_symbolic;
      addr.label := get_by_name(s);
    elsif validate_variable(s) then
      addr.mode := m_symbolic;
      addr.label := create(s);
    else
      open_pos := fix.index(s, "(");
      close_pos := fix.index(s, ")");
      if open_pos > s'first and close_pos > (open_pos + 1) then
        addr.mode := m_indexed;
        addr.value := parse_word(s(s'first..(open_pos - 1)));
        addr.register := parse_register(s((open_pos + 1)..(close_pos - 1)));
      else
        error_parse(s & " error parse operand");
      end if;
    end if;
  end parse_operand;

  function parse_operation (s : string) return record_handler_type is
  begin
    if command = null_command then
      if not validate_command(s) then
        error_parse(s & " unknown operation");
      end if;

      if endswith(s, ".b") then
        command.command := commands'value("f_" & s(s'first..(s'last - 2)));
        command.bw := false;
      elsif endswith(s, ".w") then
        command.command := commands'value("f_" & s(s'first..(s'last - 2)));
      else
        command.command := commands'value("f_" & s);
      end if;

      if command.command in two_commands then
        command.num_operands := 2;
        return (ptr => parse_operation'access);
      elsif command.command in zero_emulated_commands|f_reti then
        null;
      else
        command.num_operands := 1;
        return (ptr => parse_operation'access);
      end if;
    elsif command.source = null_operand then
      parse_operand(command.source, s);
      if command.num_operands = 2 then
        return (ptr => parse_operation'access);
      end if;
    else
      parse_operand(command.destination, s);
    end if;
    command_to_programm(command);
    command := null_command;
    return (ptr => parse_main'access);
  end parse_operation;

  function parse_raw (s : string) return record_handler_type is
  begin
    if car(s) in 'a'..'z' then
      if in_labels(s) then
        command.source.value := get(get(get_by_name(s)));
      elsif not validate_variable(s) then
        error_parse(s & " bad name");
      else
        command.source.value := get_var_val(s);
      end if;
    else
      command.source.value := parse_word(s);
    end if;
    -- command.source.value := swpb(command.source.value);
    command.source.mode := m_constant;
    command.command := f_raw;
    command.num_operands := 0;

    command_to_programm(command);
    command := null_command;

    return (ptr => parse_main'access);
  end parse_raw;

  function parse_string (s : string) return record_handler_type is
    i : natural := s'first;
  begin
    if s'length < 3 then
      error_parse(s & " error string");
    end if;
    command.source.label := null_label;
    command.source.mode := m_constant;
    command.command := f_raw;
    command.num_operands := 0;
    while i /= s'last + 1 loop
      if s(i) = '"' then
        if i /= s'last then
          error_parse("end of string before end");
        end if;
        command := null_command;
        return (ptr => parse_main'access);
      end if;
      if s(i) = '\' then
        if i = s'last then
          error_parse(" \ end of string");
        end if;
        inc(i);
      end if;
      command.source.value := character'pos(s(i));
      command_to_programm(command);
      inc(i);
    end loop;

    command := null_command;
    return (ptr => parse_string'access);
  end parse_string;

  function parse_call_macros (s : string) return record_handler_type is
  begin
    if get_macros_name_params(s) then
      getter.macros.call(unb.to_string(macro_name), unb.to_string(macro_params));
      macro_name := unb.null_unbounded_string;
      macro_params := unb.null_unbounded_string;
      return (ptr => parse_main'access);
    end if;
    return (ptr => parse_call_macros'access);
  end parse_call_macros;

  function parse_main (s : string) return record_handler_type is
  begin
    if first_element(s) = '.' then
      return parse_instruction(s);
    elsif last_element(s) = ':' then
      return parse_label(s(s'first..(s'last - 1)));
    elsif first_element(s) in 'a'..'z' then
      if fix.index(s, "(") > 1 then
        return parse_call_macros(s);
      elsif validate_command(s) then
        return parse_operation(s);
      else
        return parse_raw(s);
      end if;
    elsif first_element(s) in '0'..'9' then
      return parse_raw(s);
    elsif first_element(s) = '"' then
      return parse_string(s((s'first + 1)..s'last));
    else
      error_parse("error " & s);
    end if;

    return (ptr => parse_main'access);
  end parse_main;

  -- function preparse (s : string) return string is
  --   start_pos, end_pos, comment_pos : natural;
  -- begin
  --   comment_pos := fix.index(s, ";", ada.strings.forward);
  --   start_pos := fix.index(s, "{", ada.strings.backward);
  --   if start_pos > 0 and start_pos < comment_pos then
  --     end_pos := fix.index(s, "}", start_pos, ada.strings.forward);
  --     if end_pos < start_pos or end_pos > comment_pos then
  --       error_parse("error """ & s & """ no closed brackets");
  --     end if;
  --     return preparse(
  --       s(s'first..(start_pos-1)) &
  --       parse_precompile(s((start_pos + 1)..(end_pos - 1)) & ';') &
  --       s((end_pos + 1)..s'last));
  --   end if;
  --   return s;
  -- end preparse;
 -- вынести в модуль и сделать через финализе
  type code_t is access all unb.unbounded_string;

  type pre_code_t is record
    start_pos, end_pos : natural;
    code : code_t;
  end record;
  result : aliased unb.unbounded_string;

  -- type for_loop_t is record
  --   start_i, end_i, cur_i : natural;
  --   cursor : environment_t.cursor := environment_t.no_element;
  --   code : unb.unbounded_string;
  --   last : positive := 1;
  -- end record;
  -- for_loop_status : natural := 0;
  -- package for_loops_t is new ada.containers.vectors(element_type => for_loop_t, index_type => natural);
  -- for_loops : for_loops_t.vector;
  -- tmp_for_loop, last_for_loop : for_loop_t;
  -- for_end_offset : natural := 0;

  function pre_code_cmp (a, b : pre_code_t) return boolean is
  begin
    return false;
  end pre_code_cmp;
  package pre_codes_t is new ada.containers.doubly_linked_lists(pre_code_t, pre_code_cmp);
  pre_codes : pre_codes_t.list;

  end_asm : boolean := false;


  -- type record_getter_type;
  -- type getter_type is access function (getter : in out record_getter_type) return character;
  -- type record_getter_type is record
  --   ptr : getter_type;
  -- end record;

  -- current_file : ada.text_io.file_type;

  -- function get_from_file (getter : in out record_getter_type) return character is
  -- begin
  --   if ada.text_io.end_of_file(current_file) then
  --     return ascii.nul;
  --   end if;
  -- end get_from_file;



  function get_line return string is
    use type unb.unbounded_string;
    use type ada.containers.count_type;

    -- start_for : constant unb.unbounded_string := unb.to_unbounded_string(".for");
    -- end_for : constant unb.unbounded_string := unb.to_unbounded_string(".end_for");
    -- in_for : constant unb.unbounded_string := unb.to_unbounded_string("in");

    last_code : code_t := result'access;
    c : character;
    -- tb : boolean;
    -- dots_pos : natural;
  begin
    loop
      result := unb.null_unbounded_string;
      loop
        -- -- if not for_loops_t.is_empty(for_loops) then
        --   c := ' ';
        --   if unb.length(last_for_loop.code) < last_for_loop.last then
        --     inc(last_for_loop.cur_i);
        --     last_for_loop.last := 1;
        --     environment.replace_element(last_for_loop.cursor, word(last_for_loop.cur_i));
        --   else
        --     c := unb.element(last_for_loop.code, last_for_loop.last);
        --     inc(last_for_loop.last);
        --   end if;

        --   if last_for_loop.cur_i > last_for_loop.end_i then
        --     for_loop_status := 0;
        --     environment.delete(last_for_loop.cursor);
        --     for_loops.delete_last;
        --     if for_loops_t.length(for_loops) > 0 then
        --       last_for_loop := for_loops_t.last_element(for_loops);
        --     end if;
        --   end if;
        -- -- elsif ada.text_io.end_of_file(file) then
        -- --   c := ' ';
        -- --   end_asm := true;
        -- else
          c := getter.get;
          if c = ascii.nul then
            end_asm := true;
            return "";
          end if;
        -- end if;

        -- if for_loop_status = 4 then
        --   unb.append(tmp_for_loop.code, c);
        --   -- if ada.text_io.end_of_line(file) and for_loops_t.is_empty(for_loops) then
        --   --   unb.append(tmp_for_loop.code, ' ');
        --   -- end if;
        -- end if;

        -- \; \{ \} \  \, \\ \.
        -- if c = ';' then
        --   if not ada.text_io.end_of_line(file) then
        --     ada.text_io.skip_line(file);
        --   end if;
        --   exit when pre_codes_t.is_empty(pre_codes);
        if c = '{' then
          pre_codes.append((0, 0, new unb.unbounded_string));
          last_code := pre_codes_t.last_element(pre_codes).code;
        elsif c = '}' then
          if pre_codes_t.is_empty(pre_codes) then
            error_parse("""}"" without start");
          end if;
          if pre_codes_t.has_element(pre_codes_t.previous(pre_codes_t.last(pre_codes))) then
            unb.append(pre_codes_t.element(pre_codes_t.previous(pre_codes_t.last(pre_codes))).code.all, parse_precompile(unb.to_string(last_code.all) & ';'));
          else
            unb.append(result, parse_precompile(unb.to_string(last_code.all) & ';'));
          end if;
          -- free(last_code);
          pre_codes.delete_last;
          if pre_codes_t.is_empty(pre_codes) then
            last_code := result'access;
          else
            last_code := pre_codes_t.last_element(pre_codes).code;
          end if;
        elsif pre_codes_t.is_empty(pre_codes) and c in ' '|',' then
          exit;
        else
          unb.append(last_code.all, c);
        end if;

        -- if ada.text_io.end_of_line(file) and for_loop_status /= 5 and for_loops_t.is_empty(for_loops) then
        --   exit when pre_codes_t.is_empty(pre_codes);
        --   unb.append(last_code.all, ' ');
        -- end if;
      end loop;

      -- if for_loop_status = 1 then
      --   validate_variable_assert(unb.to_string(result));
      --   environment.insert(unb.to_string(result), 0, tmp_for_loop.cursor, tb);
      --   if not tb then
      --     error_parse("variable name """ & unb.to_string(result & """ already defined"));
      --   end if;

      --   inc(for_loop_status);
      -- elsif for_loop_status = 2 then
      --   if result /= in_for then
      --     error_parse("error format for");
      --   end if;
      --   inc(for_loop_status);
      -- elsif for_loop_status = 3 then
      --   dots_pos := unb.index(result, "..");
      --   if dots_pos < 2 or (dots_pos + 1) >= unb.length(result) then
      --     error_parse("error range: " & unb.to_string(result));
      --   end if;
      --   tmp_for_loop.start_i := natural(parse_word(unb.slice(result, 1, dots_pos - 1)));
      --   tmp_for_loop.cur_i := tmp_for_loop.start_i;
      --   tmp_for_loop.end_i := natural(parse_word(unb.slice(result, dots_pos + 2, unb.length(result))));

      --   environment.replace_element(tmp_for_loop.cursor, word(tmp_for_loop.cur_i));

      --   inc(for_loop_status);
      -- elsif result = start_for then
      --   if for_loop_status = 4 then
      --     inc(for_end_offset);
      --   else
      --     for_loop_status := 1;
      --     tmp_for_loop.code := unb.null_unbounded_string;
      --     tmp_for_loop.last := 1;
      --   end if;
      -- elsif for_loop_status = 4 then
      --   if result = end_for then
      --     if for_end_offset /= 0 then
      --       dec(for_end_offset);
      --     else
      --       unb.replace_slice(tmp_for_loop.code, unb.length(tmp_for_loop.code) - 8, unb.length(tmp_for_loop.code), " ");
      --       if for_loops_t.length(for_loops) > 0 then
      --         for_loops.replace_element(for_loops_t.last(for_loops), last_for_loop);
      --       end if;
      --       last_for_loop := tmp_for_loop;
      --       for_loops.append(last_for_loop);
      --       for_loop_status := 5;
      --     end if;
      --   end if;
      -- else
        return unb.to_string(result);
      -- end if;
    end loop;
    return "";
  end get_line;

  procedure from_file is
    -- file : ada.text_io.file_type;
    -- tmp_ustr : unb.unbounded_string;
    handler_ptr : handler_type := parse_main'access;
    -- cur_pos : natural := 0;
    -- last_space : natural := 0;
  begin
    -- getter.file.open(path);
    while not end_asm loop
      -- print(end_asm'img & path);
      declare
        l : constant string := get_line;
      begin
        -- print(l & end_asm'img);
        if l /= "" then
          handler_ptr := handler_ptr(l).ptr;
        end if;
      end;
    end loop;
    end_asm := false;

    -- ada.text_io.open(file, ada.text_io.in_file, path);
    -- while not end_asm loop
    --   declare
    --     l : constant string := get_line(file);
    --   begin
    --     if l /= "" then
    --       handler_ptr := handler_ptr(l).ptr;
    --     end if;
    --   end;
    -- end loop;
    -- end_asm := false;
    -- ada.text_io.close(file);
  exception
    when this_error: parse_error =>
      if ada.exceptions.exception_message(this_error) /= "" then
        print('[' & ltrim(getter.get_line'img) & "]: " &
              ada.exceptions.exception_message(this_error));
      end if;
      -- ada.text_io.close(file);
      raise parse_error with "";
    when ada.io_exceptions.name_error =>
      -- error_parse("no such file " & path);
      raise;
    when others =>
      -- if ada.text_io.is_open(file) then
      --   ada.text_io.close(file);
      -- end if;
      raise;
  end from_file;

  procedure update_checksum (var : in out word; val : word) is
  begin
    var := var + (val and 255);
    var := var + sr(val, 8);
  end update_checksum;

  function word_to_str_revers (val : word) return string is
  begin
    return image(sr(val, 8) or sl(val, 8), 16);
  end word_to_str_revers;

  out_text : unb.unbounded_string;
  cur : programm_t.cursor;
  line_addr : pos_addr_t;
  package line_t is new ada.strings.bounded.generic_bounded_length(max => 40);
  out_file : ada.text_io.file_type;
  words : words_t(0..18);
  size : word := 0;

  procedure command_to_words (cmd : command_t; addr : pos_addr_t) is
    operation : word := 0;
    offset : word := 0;

    procedure set_bits (pos, length : natural; val : word) is
    begin
      operation := (operation and (sl(((2 ** length) - 1), pos - length) xor ff)) or sl(val and ((2 ** length) - 1), pos - length);
    end set_bits;

    procedure repr_ext_word (op : operand_t) is
    begin
      if operand_has_ext_word(op) then
        inc(words(0));
        incd(offset);

        if op.label /= null_label and then get(op.label) = null_pos_addr then
          error_parse("label " & name(op.label) & " used but not defined");
        end if;

        if op.mode = m_absolute then
          if op.label /= null_label then
            words(words(0)) := get(get(op.label));
          else
            words(words(0)) := op.value;
          end if;
        elsif op.mode = m_immediate then
          if op.label /= null_label then
            words(words(0)) := get(get(op.label));
          else
            words(words(0)) := op.value;
          end if;
        elsif op.mode = m_symbolic then
          if op.label /= null_label then
            words(words(0)) := get(get(op.label) - addr) - offset;
          else
            words(words(0)) := op.value - get(addr) - offset;
          end if;
        else
          words(words(0)) := op.value;
        end if;
      end if;
    end repr_ext_word;

    procedure repr_destination is
    begin
      if cmd.command not in two_commands then
        return;
      end if;

      case cmd.destination.mode is
        when m_register =>
          set_bits(8, 1, 0);
          set_bits(4, 4, cmd.destination.register);
        when m_indexed =>
          set_bits(8, 1, 1);
          set_bits(4, 4, cmd.destination.register);
        when m_symbolic =>
          set_bits(8, 1, 1);
          set_bits(4, 4, r_pc);
        when m_absolute =>
          set_bits(8, 1, 1);
          set_bits(4, 4, r_sr);
        when others =>
          error_parse(cdr(cmd.destination.mode'img, 2) & " not be used for destination");
      end case;
      repr_ext_word(cmd.destination);
    end repr_destination;

    procedure source_to_operation (pos : natural) is
    begin
      case cmd.source.mode is
        when m_register|m_indexed|m_indirect_register|m_indirect_autoincrement =>
          set_bits(pos, 4, cmd.source.register);
        when m_symbolic|m_immediate =>
          set_bits(pos, 4, r_pc);
        when m_absolute =>
          set_bits(pos, 4, r_sr);
        when m_constant =>
          if cmd.source.value = 0 then
            set_bits(6, 2, 0);
            set_bits(pos, 4, 3);
          elsif cmd.source.value = 1 then
            set_bits(6, 2, 1);
            set_bits(pos, 4, 3);
          elsif cmd.source.value = 2 then
            set_bits(6, 2, 2);
            set_bits(pos, 4, 3);
          elsif cmd.source.value = 4 then
            set_bits(6, 2, 2);
            set_bits(pos, 4, r_sr);
          elsif cmd.source.value = 8 then
            set_bits(6, 2, 3);
            set_bits(pos, 4, r_sr);
          elsif cmd.source.value = -1 then
            set_bits(6, 2, 3);
            set_bits(pos, 4, 3);
          else
            error_parse("constant not constant");
          end if;
      end case;
    end source_to_operation;

    procedure repr_operands is
    begin
      if cmd.command in jump_commands|f_raw then
        return;
      end if;

      if cmd.source.mode = m_register then
        set_bits(6, 2, 0);
      elsif cmd.source.mode in m_indexed|m_symbolic|m_absolute then
        set_bits(6, 2, 1);
      elsif cmd.source.mode = m_indirect_register then
        set_bits(6, 2, 2);
      elsif cmd.source.mode in m_indirect_autoincrement|m_immediate then
        set_bits(6, 2, 3);
      end if;

      set_bits(7, 1, to_word(not cmd.bw));

      if cmd.command in single_commands then
        source_to_operation(4);
        set_bits(13, 6, commands'pos(cmd.command) - commands'pos(single_commands'first) + 32);
      elsif cmd.command in two_commands then
        source_to_operation(12);
        set_bits(16, 4, commands'pos(cmd.command) - commands'pos(two_commands'first) + 4);
      end if;
      repr_ext_word(cmd.source);

      repr_destination;
    end repr_operands;

  w : word := 0;
  begin
    if cmd.command = f_raw then
      set_bits(16, 16, cmd.source.value);
    elsif cmd.command in jump_commands then
      set_bits(16, 3, 1);
      set_bits(13, 3, commands'pos(cmd.command) - commands'pos(jump_commands'first));

      if cmd.source.mode /= m_symbolic then
        error_parse("jump operand must be symbolic");
      end if;

      if cmd.source.label = null_label then
        if odd(cmd.source.value) then
          error_parse("jump addres [" & cmd.source.value'img & " ] must be even");
        end if;
        w := sr((cmd.source.value - get(addr) - 2), 1);
      elsif get(cmd.source.label) = null_pos_addr then
        error_parse("label " & name(cmd.source.label) & " used but not defined");
      else
        w := sr(get(get(cmd.source.label) - addr - create(2)), 1);
      end if;

      if w in 16#200#..16#7dff# then
        if cmd.source.label /= null_label then
          error_parse("jump to " & name(cmd.source.label) & " address > 10bit");
        else
          error_parse("jump to " & cmd.source.value'img & " address > 10bit");
        end if;
      end if;
      set_bits(10, 10, w);
    end if;

    inc(words(0));
    w := words(0);
    repr_operands;
    words(w) := operation;
  end command_to_words;

  procedure append_line is
    l : word := word'min(words(0), 8);
    checksum : word;
  begin
    if l = 0 then
      return;
    end if;
    size := size + l * 2;
    checksum := l * 2;
    update_checksum(checksum, get(line_addr));
    unb.append(out_text, ':' & cdr(image(l * 2, 16), 2) & image(get(line_addr), 16) & "00");
    for i in 1..l loop
      update_checksum(checksum, words(i));
      unb.append(out_text, word_to_str_revers(words(i)));
    end loop;
    unb.append(out_text, cdr(image(256 - (checksum and 255), 16), 2) & ascii.cr & ascii.lf);
    if words(0) > 8 then
      for i in 9..words(0) loop
        words(i - 8) := words(i);
      end loop;
      words(0) := words(0) - 8;
      line_addr := line_addr + word(16);
    else
      if programm_t.has_element(cur) then
        line_addr := programm_t.key(cur);
      end if;
      words(0) := 0;
    end if;
  end append_line;

  reti_inserted : boolean := false;
  reti_cursor : programm_t.cursor;
  reti_address : pos_addr_t;
begin
  print(" 0xfa [http://0xfa.space]");
  print("a0xfa v0.1.0 - msp430 assembler (ihex format [https://wikipedia.org/wiki/Intel_HEX])");
  print("");

  if word'size > ada.containers.hash_type'size then
    error("hash type error");
  end if;
  if word'size > natural'size then
    error("integer size error");
  end if;

  if ada.command_line.argument_count /= 2 then
    error("format: a0xfa input_file_path output_file_path");
  end if;
  getter.file.open(ada.command_line.argument(1));
  from_file;

  reti_address := create(get_var_val("mem_code_end") - 1);
  loop
    programm_t.insert(programm, reti_address, (f_reti, null_operand, null_operand, 0, true), reti_cursor, reti_inserted);
    exit when reti_inserted;
    dec(reti_address);
  end loop;
  for i in 32736..32767 loop
    programm_t.insert(programm, address.create(word(i * 2)), (f_raw, (m_constant, get(reti_address), 0, null_label), null_operand, 0, true), reti_cursor, reti_inserted);
  end loop;

  cur := programm.first;
  line_addr := programm.first_key;
  words(0) := 0;
  while programm_t.has_element(cur) loop
    command_to_words(programm_t.element(cur), programm_t.key(cur));
    programm_t.next(cur);
    if words(0) >= 8 then
      append_line;
    end if;
    if not programm_t.has_element(cur) or else programm_t.key(cur) /= (line_addr + words(0) * 2) then
      append_line;
    end if;
  end loop;
  unb.append(out_text, ":00000001FF");

  ada.text_io.create(out_file, name => ada.command_line.argument(2));
  ada.text_io.put(out_file, unb.to_string(out_text));
  ada.text_io.close(out_file);

  print("done. size:" & size'img & "b");

exception
  when this_error: parse_error =>
    error(ada.exceptions.exception_message(this_error));
  when this_error: ada.io_exceptions.use_error =>
    error("Error: " & ada.exceptions.exception_message(this_error));
end a0xfa;
