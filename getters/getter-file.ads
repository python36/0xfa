with ada.text_io;
with ada.containers.vectors;
with ada.unchecked_deallocation;

with strings; use strings;

package getter.file is
  function get return character;
  procedure open (path : string);

private
  type descriptor_access_t is access ada.text_io.file_type;
  type file_t is record
    descriptor : descriptor_access_t;
    line : pos_count;
  end record;

  procedure free is new ada.unchecked_deallocation
    (object => ada.text_io.file_type, name => descriptor_access_t);

  package files_stack_t is new ada.containers.vectors(
    element_type => file_t, index_type => natural);
  files_stack : files_stack_t.vector;

  current_ptr : descriptor_access_t;
  current_line : pos_count;

  sended_last_lf : boolean := false;
end getter.file;