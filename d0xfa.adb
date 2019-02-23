with ada.text_io;
with ada.command_line;
with ada.strings.fixed;
with gnat.os_lib;
with ada.containers.vectors;

with numbers; use numbers;
with strings; use strings;

procedure d0xfa is
  use type ada.text_io.count;
  use type byte;
  use type word;

  PROGRAMM_NAME : constant string := "d0xfa";

  file : ada.text_io.file_type;

  procedure close_app is
  begin
    if ada.text_io.is_open(file) then
      ada.text_io.close(file);
    end if;
  end close_app;

  procedure error (s : string) is
  begin
    close_app;
    print(s);
    gnat.os_lib.os_exit(1);
  end error;

  subtype label_t is string(1..6);
  null_label : constant label_t := (others => ' ');
  label_counter : natural := 1;
  interrupt_lines_counter : natural := 0;
  must_be_next_addr : word := 0;

  type commands is (
    f_rrc, f_swpb, f_rra, f_sxt, f_push, f_call, f_reti,
    f_jne, f_jeq, f_jnc, f_jc, f_jn, f_jge, f_jl, f_jmp,
    f_mov, f_add, f_addc, f_subc, f_sub, f_cmp, f_dadd,
      f_bit, f_bic, f_bis, f_xor, f_and,
    f_jnz, f_jz, f_jlo, f_jhs);
  subtype single_commands is commands range f_rrc..f_reti;
  subtype jump_commands is commands range f_jne..f_jmp;
  subtype two_commands is commands range f_mov..f_and;

  type command_types is (t_jump, t_single, t_two, t_interrupt);
  type addressing_modes is (
    m_constant,
    m_immediate, m_indexed, m_symbolic, m_absolute,
    m_indirect_register, m_indirect_autoincrement, m_register);

  type address_t is record
    mode : addressing_modes := m_constant;
    address : word := 0;
    register : word := 0;
    ext_word : boolean := false;
    label : label_t := null_label;
  end record;

  type command_t is record
    command_raw : word;
    command : commands;
    command_type : command_types;
    address : word;
    first_op, second_op : word := 0;
    source, destination : address_t;
    label : label_t := null_label;
    num_operands : byte := 0;
    bw : boolean := true;
  end record;

  tmp_command : command_t;

  package programm_t is new ada.containers.vectors(
    element_type => command_t, index_type => natural);
  programm : programm_t.vector;
  programm_cursor : programm_t.cursor;

  procedure parse_source (cmd : in out command_t) is
    as : word := sr(cmd.command_raw, 4) and 3;
    src : word;
  begin
    if cmd.command_type = t_single then
      src := cmd.command_raw and 15;
    else
      src := sr(cmd.command_raw, 8) and 15;
    end if;

    cmd.source.register := src;
    cmd.source.address := src;
    if as = 0 then
      if src = 3 then
        cmd.source.mode := m_constant;
        cmd.source.address := 0;
      else
        cmd.source.mode := m_register;
      end if;
    elsif as = 1 then
      if src = 0 then
        cmd.source.mode := m_symbolic;
      elsif src = 2 then
        cmd.source.mode := m_absolute;
      elsif src = 3 then
        cmd.source.mode := m_constant;
        cmd.source.address := 1;
      else
        cmd.source.mode := m_indexed;
      end if;
    elsif as = 2 then
      if src = 2 then
        cmd.source.mode := m_constant;
        cmd.source.address := 4;
      elsif src = 3 then
        cmd.source.mode := m_constant;
        cmd.source.address := 2;
      else
        cmd.source.mode := m_indirect_register;
      end if;
    elsif as = 3 then
      if src = 0 then
        cmd.source.mode := m_immediate;
      elsif src = 2 then
        cmd.source.mode := m_constant;
        cmd.source.address := 8;
      elsif src = 3 then
        cmd.source.mode := m_constant;
        cmd.source.address := 16#ffff#;
      else
        cmd.source.mode := m_indirect_autoincrement;
      end if;
    end if;

    if cmd.source.mode in m_immediate..m_absolute then
      inc(cmd.num_operands);
      cmd.source.ext_word := true;
    end if;
  end parse_source;

  procedure parse_destination (cmd : in out command_t) is
    ad : word := sr(cmd.command_raw, 7) and 1;
    dst : word := cmd.command_raw and 15;
  begin
    cmd.destination.register := dst;
    if ad = 0 then
      cmd.destination.mode := m_register;
      cmd.destination.address := dst;
    else
      inc(cmd.num_operands);
      cmd.destination.ext_word := true;
      if dst = 0 then
        cmd.destination.mode := m_symbolic;
      elsif dst = 2 then
        cmd.destination.mode := m_absolute;
      else
        cmd.destination.mode := m_indexed;
      end if;
    end if;
  end parse_destination;

  procedure parse_single (cmd : in out command_t) is
  begin
    cmd.command := commands'val(
      (sr(cmd.command_raw, 7) and 7) +
      word(single_commands'pos(single_commands'first)));
    parse_source(cmd);
  end parse_single;

  procedure parse_two (cmd : in out command_t) is
  begin
    cmd.command := commands'val(
      (sr(cmd.command_raw, 12) +
      word(two_commands'pos(two_commands'first)) - 4));
    parse_source(cmd);
    parse_destination(cmd);
  end parse_two;

  procedure parse_jump (cmd : in out command_t) is
  begin
    cmd.command := commands'val(
      ((sr(cmd.command_raw, 10) and 7) +
      word(jump_commands'pos(jump_commands'first))));
  end parse_jump;

  procedure put_operand (op : address_t; op_m : addressing_modes) is
  begin
    if op_m = m_register then
      put('r' & ltrim(op.address'img));
    elsif op_m = m_indexed then
      put(ltrim(word_to_integer(op.address)'img) & "(r" & ltrim(op.register'img) & ')');
    elsif op_m = m_constant then
      put('#' & ltrim(word_to_integer(op.address)'img));
    elsif op_m = m_symbolic then
      if op.label /= null_label then
        put(rtrim(op.label));
      else
        put("$");
        if word_to_integer(op.address) >= 0 then
          put("+");
        end if;
        put(ltrim(word_to_integer(op.address)'img));
      end if;
    elsif op_m = m_absolute then
      if op.label /= null_label then
        put('&' & rtrim(op.label));
      else
        put("&" & hex(op.address));
      end if;
    elsif op_m = m_indirect_register then
      put("@r" & ltrim(op.register'img));
    elsif op_m = m_indirect_autoincrement then
      put("@r" & ltrim(op.register'img) & "+");
    elsif op_m = m_immediate then
      put("#" & ltrim(word_to_integer(op.address)'img));
    end if;
  end put_operand;

  function new_command (w, addr : word) return command_t is
    cmd : command_t;
  begin
    cmd.command_raw := w;
    cmd.address := addr;

    if addr >= 16#ffc0# then
      cmd.command_type := t_interrupt;
      return cmd;
    end if;

    cmd.bw := (w and 64) /= 0;
    case sr(cmd.command_raw, 13) is
      when 0 => cmd.command_type := t_single; parse_single(cmd);
      when 1 => cmd.command_type := t_jump; cmd.bw := false; parse_jump(cmd);
      when others => cmd.command_type := t_two; parse_two(cmd);
    end case;

    return cmd;
  end new_command;

  procedure operand_to_command (cmd : in out command_t; w : word; tl : byte) is
  begin
    if cmd.num_operands = 2 then
      if tl = 2 then
        cmd.source.address := w;
      else
        cmd.destination.address := w;
      end if;
    elsif cmd.source.ext_word then
      cmd.source.address := w;
    else
      cmd.destination.address := w;
    end if;
  end operand_to_command;

  function get_addr_by_offset (cmd : command_t) return word is
  begin
    return ((cmd.command_raw and 1023) + 64512 * (
      sr(cmd.command_raw, 9) and 1) + 1) * 2 + cmd.address;
  end get_addr_by_offset;

  procedure create_label (cmd : command_t; addr : in out address_t) is
    tmp_addr : word := addr.address;
    tmp_cursor : programm_t.cursor;
    tmp_command : command_t;
  begin
    if cmd.command_type = t_jump then
      tmp_addr := get_addr_by_offset(cmd);
    elsif addr.mode = m_symbolic then
      tmp_addr := tmp_addr + 2 + cmd.address;
      if addr = cmd.destination and cmd.source.ext_word then
        tmp_addr := tmp_addr + 2;
      end if;
    elsif addr.mode /= m_absolute then
      error("programm error");
    end if;

    tmp_cursor := programm.first;
    while (programm_t.has_element(tmp_cursor)) loop
      tmp_command := programm_t.element(tmp_cursor);
      if tmp_command.address = tmp_addr then
        if tmp_command.label = null_label then
          tmp_command.label := ada.strings.fixed.head(
            'l' & ltrim(label_counter'img), null_label'length);
          inc(label_counter);
          programm_t.replace_element(programm, tmp_cursor, tmp_command);
        end if;
        addr.label := tmp_command.label;
        exit;
      end if;
      tmp_cursor := programm_t.next(tmp_cursor);
    end loop;

  end create_label;

  procedure end_command (cmd : in out command_t) is
    tmp_addr : address_t;
  begin
    if cmd.command_type = t_jump then
      create_label(cmd, cmd.destination);
    else
      if cmd.source.mode in m_symbolic|m_absolute then
        create_label(cmd, cmd.source);
      end if;
      if cmd.destination.mode in m_symbolic|m_absolute then
        create_label(cmd, cmd.destination);
      end if;
    end if;

    programm.append(cmd);
  end end_command;

  procedure disasm (cmd : in out command_t) is
    tmp_cur : programm_t.cursor;
    col : ada.text_io.count := 1;

    procedure shift_col (v : ada.text_io.count) is
    begin
      col := col + v;
      ada.text_io.set_col(col);
    end shift_col;

    procedure set_col (v : ada.text_io.count) is
    begin
      col := v;
      ada.text_io.set_col(col);
    end set_col;

    procedure put_hex_raw (w : word) is
      strword : string(1..4) := image(w, 16);
    begin
      put(strword(3..4) & ' ' & strword(1..2));
    end put_hex_raw;

    function repr_bw return string is
    begin
      if cmd.bw then
        return ".b";
      end if;
      return "";
    end repr_bw;

    function repr_commands (command : commands) return string is
    begin
      return lowercase(command'img(3..command'img'length)) & repr_bw;
    end repr_commands;

    procedure put_alternative (scmd : string) is
    begin
      put("; " & scmd);
      shift_col(9);
    end put_alternative;

    procedure put_alternative_with_dst (scmd : string) is
    begin
      put_alternative(scmd);
      put_operand(cmd.destination, cmd.destination.mode);
    end put_alternative_with_dst;

    procedure put_jump_offset (jcmd : command_t) is
    begin
      if jcmd.destination.label = null_label then
        put("$");
        if (sr(cmd.command_raw, 9) and 1) = 0 then
          put("+");
        end if;
        put(ltrim(word_to_integer(
          ((jcmd.command_raw and 1023) + 64512 * (
            sr(cmd.command_raw, 9) and 1) + 1) * 2)'img));
      else
        put(jcmd.destination.label);
      end if;
    end put_jump_offset;

    procedure put_comment (sc : string) is
    begin
      put("; " & sc);
    end put_comment;

    procedure put_label (s : string) is
    begin
      put(ada.strings.fixed.head(
        rtrim(s) & ':', null_label'length));
    end put_label;
  begin
    if cmd.source.mode in m_symbolic|m_absolute then
      create_label(cmd, cmd.source);
    end if;
    if cmd.destination.mode in m_symbolic|m_absolute then
      create_label(cmd, cmd.destination);
    end if;
    if cmd.command_type = t_jump then
      create_label(cmd, cmd.destination);
    end if;

    if cmd.label /= null_label then
      put_label(cmd.label);
    end if;
    shift_col(7);

    put(repr_commands(cmd.command));
    shift_col(7);
    if cmd.command_type = t_jump then
      put_jump_offset(cmd);
      if cmd.command in f_jne..f_jc then
        shift_col(24);
        put_alternative(repr_commands(
          commands'val(commands'pos(cmd.command) + (
            commands'pos(f_jnz) - commands'pos(f_jne)))));
        put_jump_offset(cmd);
      end if;
    elsif cmd.command /= f_reti then
      put_operand(cmd.source, cmd.source.mode);
    end if;
    if cmd.command_type = t_two then
      put(",");
      shift_col(12);
      put_operand(cmd.destination, cmd.destination.mode);

      shift_col(12);
      if cmd.command = f_addc then
        if cmd.source.mode = cmd.destination.mode and
            cmd.source.address = cmd.destination.address then
          put_alternative_with_dst("rlc" & repr_bw);
        elsif cmd.source.mode = m_constant and cmd.source.address = 0 then
          put_alternative_with_dst("adc" & repr_bw);
        end if;
      elsif cmd.command = f_add then
        if cmd.source.mode = cmd.destination.mode and
            cmd.source.address = cmd.destination.address then
          put_alternative_with_dst("rla" & repr_bw);
        elsif cmd.source.mode = m_constant then
          if cmd.source.address = 1 then
            put_alternative_with_dst("inc" & repr_bw);
          elsif cmd.source.address = 2 then
            put_alternative_with_dst("incd" & repr_bw);
          end if;
        end if;
      elsif cmd.command = f_xor and cmd.source.mode = m_constant and cmd.source.address = 16#ffff# then
        put_alternative_with_dst("inv" & repr_bw);
      elsif cmd.command = f_bic and cmd.destination.mode = m_register and
          cmd.destination.register = 2 and cmd.source.mode = m_constant then
        if cmd.source.address = 1 then
          put_alternative("clrc");
        elsif cmd.source.address = 2 then
          put_alternative("clrz");
        elsif cmd.source.address = 4 then
          put_alternative("clrn");
        elsif cmd.source.address = 8 then
          put_alternative("dint");
        end if;
      elsif cmd.command = f_bis and cmd.source.mode = m_constant and
          cmd.destination.mode = m_register and cmd.destination.register = 2 then
        if cmd.source.address = 1 then
          put_alternative("setc");
        elsif cmd.source.address = 2 then
          put_alternative("setz");
        elsif cmd.source.address = 4 then
          put_alternative("setn");
        elsif cmd.source.address = 8 then
          put_alternative("eint");
        end if;
      elsif cmd.command = f_cmp and
          cmd.source.mode = m_constant and cmd.source.address = 0 then
        put_alternative_with_dst("tst" & repr_bw);
      elsif cmd.command = f_dadd and
          cmd.source.mode = m_constant and cmd.source.address = 0 then
        put_alternative_with_dst("dadc" & repr_bw);
      elsif cmd.command = f_subc and
          cmd.source.mode = m_constant and cmd.source.address = 0 then
        put_alternative_with_dst("sbc" & repr_bw);
      elsif cmd.command = f_sub and
          cmd.source.mode = m_constant then
        if cmd.source.address = 1 then
          put_alternative_with_dst("dec" & repr_bw);
        elsif cmd.source.address = 2 then
          put_alternative_with_dst("decd" & repr_bw);
        end if;
      elsif cmd.command = f_mov then
        if cmd.source.mode = m_indirect_autoincrement and cmd.source.address = 1 then
          if cmd.destination.mode = m_register and cmd.destination.address = 0 then
            put_alternative("ret");
          else
            put_alternative_with_dst("pop" & repr_bw);
          end if;
        elsif cmd.destination.mode = m_register and cmd.destination.address = 0 then
          put_alternative("br");
          put_operand(cmd.source, cmd.source.mode);
        elsif cmd.source.mode = m_constant and cmd.source.address = 0 then
          if cmd.destination.mode = m_register and cmd.destination.register = 3 then
            put_alternative("nop");
          else
            put_alternative_with_dst("clr" & repr_bw);
          end if;
        end if;
      end if;
    end if;

    set_col(59);
    if cmd.command_type = t_jump then
        put_comment(hex(get_addr_by_offset(cmd)));
    else
      if cmd.source.mode = m_immediate then
        put_comment('#' & hex(cmd.source.address));
      elsif cmd.source.mode = m_constant and cmd.source.address = 16#ffff# then
        put_comment('#' & hex(cmd.source.address));
      elsif cmd.source.mode = m_symbolic then
        put_comment(hex(cmd.address + 2 + cmd.source.address));
      elsif cmd.source.mode = m_absolute then
        put_comment('&' & hex(cmd.source.address));
      end if;

      if cmd.destination.mode = m_symbolic then
        put_comment(hex(cmd.address + word(cmd.num_operands) * 2 + cmd.destination.address));
      elsif cmd.destination.mode = m_absolute then
        put_comment('&' & hex(cmd.destination.address));
      end if;
    end if;

    set_col(78);
    put("; ");
    put(image(cmd.address, 16));
    shift_col(9);
    put_hex_raw(cmd.command_raw);
    if cmd.source.ext_word then
      put("  ");
      put_hex_raw(cmd.source.address);
    end if;
    if cmd.destination.ext_word then
      put("  ");
      put_hex_raw(cmd.destination.address);
    end if;

    ada.text_io.new_line;
  end disasm;

  i : byte;
  cmd : command_t;
  l, op_l : byte := 0;
  is_ssar : boolean := false;
  is_end : boolean := true;
  is_first : boolean := true;
  checksum : byte;
  first_char : character;
  b : byte;
  wt : word;
  addr, next_addr, op_addr : word := 0;

begin
  print(";;; 0xfa [http://0xfa.space] ;;;");
  print("; d0xfa v0.1.0 - msp430 disassembler (ihex format [https://wikipedia.org/wiki/Intel_HEX])");
  print("");
  if ada.command_line.argument_count = 0 then
    error("no input files");
  end if;
  ada.text_io.open(file, ada.text_io.in_file, ada.command_line.argument(1));
  loop_read: while not ada.text_io.end_of_file(file) loop
    if l /= 0 or not is_end then
      error("bad format");
    end if;
    i := 0;
    checksum := 0;
    is_end := false;
    for c of remove_ret_car(ada.text_io.get_line(file)) loop
      if not (c in '0'..'9' or c in 'A'..'F' or (i = 0 and c = ':')) then
        error("bad format");
      end if;
      if (i and 1) /= 0 then
        first_char := c;
      elsif i /= 0 then
        b := byte'value("16#" & first_char & c & '#');
        checksum := checksum + b;
        if i = 2 then
          l := b;
        elsif i = 4 then
          addr := word(b);
        elsif i = 6 then
          addr := sl(addr, 8) + word(b);
          if next_addr > 0 and next_addr - word(op_l) * 2 /= addr then
            if op_l /= 0 then
              error("seq addrs");
            end if;
          end if;
          if op_l = 0 then
            next_addr := addr;
            op_addr := next_addr;
          end if;
        elsif i = 8 then
          if b = 1 then
            if l /= 0 or addr /= 0 then
              error("bad end record");
            end if;
            exit loop_read;
          elsif b = 3 then
            if l /= 4 or addr /= 0 then
              error("bad SSAR len");
            end if;
            is_ssar := true;
          end if;
        elsif i > 9 and l > 0 then
          if is_first then
            wt := word(b);
            is_first := false;
          else
            wt := sl(word(b), 8) or wt;
            is_first := true;
          end if;

          if is_first then
            if op_l = 0 then
              if is_ssar then
                op_l := 1;
              else
                cmd := new_command(wt, op_addr);
                op_l := cmd.num_operands;
                next_addr := op_addr + 2 + word(op_l) * 2;
              end if;
            else
              if is_ssar then
                ada.text_io.set_col(1);
                print("; SSAR: " & hex(sl(wt, 8) + sr(wt, 8)));
              else
                operand_to_command(cmd, wt, op_l);
              end if;
              dec(op_l);
            end if;

            if op_l = 0 then
              if not is_ssar then
                end_command(cmd);
                op_addr := next_addr;
              end if;
            end if;
          end if;

          dec(l);
        elsif i > 9 and l = 0 then
          is_end := true;
          if checksum /= 0 then
            error("checksum error");
          end if;
        end if;
      end if;

      inc(i);
    end loop;
  end loop loop_read;

  print(";label cmd    1st_operand 2nd_operand  emulated             comments           addr   raw");

  programm_cursor := programm.first;
  while (programm_t.has_element(programm_cursor)) loop
    tmp_command := programm_t.element(programm_cursor);
    if tmp_command.command_type = t_interrupt then
      if interrupt_lines_counter = 0 then
        print("; interrupt vectors:");
        put(';');
        interrupt_lines_counter := 1;
      end if;
      if tmp_command.address >= 16#fff0# and interrupt_lines_counter = 1 then
        interrupt_lines_counter := 2;
        ada.text_io.new_line;
        put(';');
      end if;
      ada.text_io.set_col((ada.text_io.count(tmp_command.address - 16#ffe0#) mod 16) * 6 + 3);
      put(image(tmp_command.address, 16) & ": " & image(tmp_command.command_raw, 16) & " ");
    else
      if must_be_next_addr /= 0 and must_be_next_addr /= tmp_command.address then
        print("; ...");
      end if;
      disasm(tmp_command);
      must_be_next_addr := tmp_command.address + 2 * word(tmp_command.num_operands) + 2;
    end if;
    programm_cursor := programm_t.next(programm_cursor);
  end loop;

  close_app;
end d0xfa;