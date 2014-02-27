class Main inherits IO {
  main() : Int {
    let
      a : Int <- 6
    in 
      a <- a@P.findAnswer()
  } ;
} ;

Class P {
	findAnswer() : Bool { false };
};

