with ada.containers.indefinite_ordered_maps;

with strings; use strings;
with address; use address;

package labels is
  type label_t is tagged private;
  null_label : constant label_t;

  error_label_exist : exception;
  error_label_is_null : exception;

  function last_label return label_t;
  function get_by_name (str : string) return label_t;
  procedure create (str : string; addr : pos_addr_t := null_pos_addr);
  function create (str : string; addr : pos_addr_t := null_pos_addr) return label_t;
  procedure set (label : label_t; addr : pos_addr_t);
  function set (label : label_t; addr : pos_addr_t) return label_t;
  function get (label : label_t) return pos_addr_t;
  function name (label : label_t) return string;

  procedure void (label : label_t);

private
  package labels_t is new ada.containers.indefinite_ordered_maps(
    element_type => pos_addr_t, key_type => string);
  labels : labels_t.map;

  use type labels_t.cursor;

  type label_t is tagged record
    cursor : labels_t.cursor;
  end record;

  null_label : constant label_t := (cursor => labels_t.no_element);
  last : label_t := null_label;

end labels;