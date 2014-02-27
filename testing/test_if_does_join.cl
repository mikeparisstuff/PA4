class Main {
    main() : Int {
        let b : C <- if true then new B else new C fi in
            b
    };
};

Class B {};
Class C inherits B {};
