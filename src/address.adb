package body address is
  function valid_value_for_pos_addr (value : word) return boolean is
  begin
    return even(value) and value /= 16#ffff#;
  end valid_value_for_pos_addr;

  procedure valid_addr_assert (addr : word) is
  begin
    if not valid_value_for_pos_addr(addr) then
      raise error_address_odd;
    end if;
  end valid_addr_assert;

  function create (value : word) return pos_addr_t is
  begin
    valid_addr_assert(value);
    return (addr => value);
  end create;

  procedure set (pos_addr : in out pos_addr_t; value : word) is
  begin
    valid_addr_assert(value);
    pos_addr.addr := value;
  end set;

  function get (pos_addr : pos_addr_t) return word is
  begin
    return pos_addr.addr;
  end get;

  procedure inc (pos_addr : in out pos_addr_t) is
  begin
    incd(pos_addr.addr);
    valid_addr_assert(pos_addr.addr);
  end inc;

  procedure dec (pos_addr : in out pos_addr_t) is
  begin
    decd(pos_addr.addr);
    valid_addr_assert(pos_addr.addr);
  end dec;

  function inc (pos_addr : pos_addr_t) return pos_addr_t is
  begin
    valid_addr_assert(incd(pos_addr.addr));
    return (addr => incd(pos_addr.addr));
  end inc;

  function "<" (a, b : pos_addr_t) return boolean is
  begin
    return a.addr < b.addr;
  end "<";

  function "<=" (a, b : pos_addr_t) return boolean is
  begin
    return a.addr <= b.addr;
  end "<=";

  function ">" (a, b : pos_addr_t) return boolean is
  begin
    return a.addr > b.addr;
  end ">";

  function ">=" (a, b : pos_addr_t) return boolean is
  begin
    return a.addr >= b.addr;
  end ">=";

  function "-" (a, b : pos_addr_t) return pos_addr_t is
  begin
    return (addr => a.addr - b.addr);
  end "-";

  function "+" (a, b : pos_addr_t) return pos_addr_t is
  begin
    return (addr => a.addr + b.addr);
  end "+";

  function "*" (a, b : pos_addr_t) return pos_addr_t is
  begin
    return (addr => a.addr * b.addr);
  end "*";

  function "/" (a, b : pos_addr_t) return pos_addr_t is
  begin
    return (addr => a.addr / b.addr);
  end "/";

  function "+" (a: pos_addr_t; b : natural) return pos_addr_t is
  begin
    return (addr => a.addr + word(b));
  end "+";

  function "+" (a: pos_addr_t; b : word) return pos_addr_t is
  begin
    return (addr => a.addr + b);
  end "+";


end address;