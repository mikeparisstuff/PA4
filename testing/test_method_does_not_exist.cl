class Main inherits IO {

  main() : Object {
    let
      p : P <- new P
    in {
      p.getX();
      p.findTheMeaningToLifeAndEverything();
    }
  } ;
} ;

Class P {
  x : Int <- 0;
  getX() : Int { x };
};
