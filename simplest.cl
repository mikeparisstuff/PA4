class Main inherits IO {
  main() : Object {
       let x : Int <- 5555555,
       y : String <- "Hello",
       z : Int in
       "5"
   };
   woof(s : String) : Object {
   	s
   };
} ; 

class A inherits Main {
	call() : Object {
		-- self@Main.woof("hi")
		case false of t : Int => 5 ; t2 : String => "Hi"; esac
	};
};