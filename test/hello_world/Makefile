all:
	./fly < $(file)
	g++ -pthread -o test_build -std=c++11 tmp.cc
	./test_build
