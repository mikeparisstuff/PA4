class Silly {
	notAnInstanceOfSelf : Int <- 5;
	lies() : SELF_TYPE { notAnInstanceOfSelf };
};

class Main {
	main() : Object { 5 };
};
