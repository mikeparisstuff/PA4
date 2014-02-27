class Silly {
  name : String;

  get_name() : String { name };
};

class Sally inherits Silly {
  name : String;

  get_name() : String { name };
};

class Main {
  x : Sally <- (new Sally);

  main() : String { x.get_name() };
};
