package body env is

  function validate_variable (s : string) return boolean is
  begin
    return gnat.regexp.match(s, variable_regexp);
  end validate_variable;

end env;