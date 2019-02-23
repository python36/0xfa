package body labels is
  function last_label return label_t is
  begin
    return last;
  end last_label;

  function get_by_name (str : string) return label_t is
    cur : labels_t.cursor := labels_t.find(labels, str);
  begin
    if cur = labels_t.no_element then
      return null_label;
    end if;
    return (cursor => cur);
  end get_by_name;

  function create (str : string; addr : pos_addr_t := null_pos_addr) return label_t is
    cur : labels_t.cursor;
    tb : boolean;
  begin
    labels_t.insert(labels, str, addr, cur, tb);
    if not tb then
      raise error_label_exist;
    end if;
    last := (cursor => cur);
    return last_label;
  end create;

  procedure create (str : string; addr : pos_addr_t := null_pos_addr) is
  begin
    void(create(str, addr));
  end create;

  procedure set (label : label_t; addr : pos_addr_t) is
  begin
    if label.cursor = labels_t.no_element then
      raise error_label_is_null;
    end if;
    labels_t.replace_element(labels, label.cursor, addr);
  end set;

  function set (label : label_t; addr : pos_addr_t) return label_t is
  begin
    set(label, addr);
    return label;
  end set;
  
  function get (label : label_t) return pos_addr_t is
  begin
    if label.cursor = labels_t.no_element then
      raise error_label_is_null;
    end if;
    return labels_t.element(label.cursor);
  end get;

  function name (label : label_t) return string is
  begin
    if label.cursor = labels_t.no_element then
      raise error_label_is_null;
    end if;
    return labels_t.key(label.cursor);
  end name;

  procedure void (label : label_t) is
  begin
    null;
  end void;

end labels;