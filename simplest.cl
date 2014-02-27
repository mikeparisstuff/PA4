class Main inherits IO {
  my_attribute : Int <- 5 ;
  method1(num : Int) : SELF_TYPE {  -- same
      self
   };
   class_type(var : A) : SELF_TYPE {
      case var of
   a : A => out_string("Class type is now A\n");
   b : B => out_string("Class type is now B\n");
   c : C => out_string("Class type is now C\n");
   d : D => out_string("Class type is now D\n");
   e : E => out_string("Class type is now E\n");
   o : Object => out_string("Oooops\n");
      esac
   };
  main() : Object { 
	    while not position < num loop
        {
            temp <- self.abort();
            position <- position + 1 * 3 / 2 - 8;
        }
        pool
  } ;
} ; 