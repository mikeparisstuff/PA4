class Main inherits IO {
  main() : SELF_TYPE {
        out_string("asdf")
   };
} ; 


class Comp inherits IO{
    y : Int;
    x : Int;

    print() : Object {
        if y = 0
            then out_int(x)
            else out_int(x)
        fi
    };


}; 
