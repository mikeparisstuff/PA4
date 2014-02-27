class Main inherits IO {
  main() : Int {
    let
      p : P <- new P,
      q : Q <- new Q,
      a : Int
    in {
      a <- a@P.getX();
    }
  } ;
} ;

Class P {
	x : Int <- 0;
	y : Int <- 0;

	getX() : Int { x };
};

Class Q inherits P {
  someVal : Int <- 10;

  getX() : Int { someVal };
};
