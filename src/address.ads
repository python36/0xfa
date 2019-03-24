with numbers; use numbers;
with strings; use strings;

package address is
  type pos_addr_t is tagged private;
  null_pos_addr : constant pos_addr_t;

  error_address_odd : exception;

  function create (value : word) return pos_addr_t;
  procedure set (pos_addr : in out pos_addr_t; value : word);
  function get (pos_addr : pos_addr_t) return word;
  function valid_value_for_pos_addr (value : word) return boolean;
  procedure inc (pos_addr : in out pos_addr_t);
  function inc (pos_addr : pos_addr_t) return pos_addr_t;
  procedure dec (pos_addr : in out pos_addr_t);

  function "<" (a, b : pos_addr_t) return boolean;
  function "<=" (a, b : pos_addr_t) return boolean;
  function ">" (a, b : pos_addr_t) return boolean;
  function ">=" (a, b : pos_addr_t) return boolean;

  function "-" (a, b : pos_addr_t) return pos_addr_t;
  function "+" (a, b : pos_addr_t) return pos_addr_t;
  function "*" (a, b : pos_addr_t) return pos_addr_t;
  function "/" (a, b : pos_addr_t) return pos_addr_t;
  function "+" (a: pos_addr_t; b : natural) return pos_addr_t;
  function "+" (a: pos_addr_t; b : word) return pos_addr_t;

private
  use type word;

  type pos_addr_t is tagged record
    addr : word;
  end record;

  null_pos_addr : constant pos_addr_t := (addr => 16#ffff#);

end address;