with ada.containers.vectors;

with numbers; use numbers;
with strings; use strings;
with env; use env;

package getter.for_loop is
  ERROR_NO_CLOSED : exception;

  function get return character;
  procedure create (start_i, end_i : word; var : environment_t.cursor);

private
  end_for_statement : constant string := ".end_for";
  for_statement : constant string := ".for";

  type for_loop_t is record
    start_i, end_i, cur_i : natural;
    cursor : environment_t.cursor := environment_t.no_element;
    code : unb.unbounded_string;
    last : positive := 1;
    first_line, cur_line : pos_count;
  end record;

  package for_loops_t is new ada.containers.vectors(element_type => for_loop_t, index_type => natural);
  for_loops : for_loops_t.vector;

  cur_for_loop : for_loop_t;
  tmp_for_loop : for_loop_t;
end getter.for_loop;