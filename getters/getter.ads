with ada.text_io;
with ada.containers.vectors;

package getter is
  ERROR_NO_GETTERS : exception;

  subtype pos_count is ada.text_io.positive_count;

  -- type record_getter_type;
  -- type getter_type is access function return character;
  -- type record_getter_type is record
  --   ptr : getter_type;
  -- end record;

  function get (lf2space : boolean := true) return character;
  function get_line return pos_count; -- inline

private
  type getter_type is access function return character;
  procedure push (getter : getter_type);
  procedure pop;

  procedure set_line (cnt : pos_count); -- inline
  current_line : pos_count := pos_count'first;

  package getters_stack_t is new ada.containers.vectors(
    element_type => getter_type, index_type => natural);
  getters_stack : getters_stack_t.vector;

  current_getter : getter_type;
end getter;