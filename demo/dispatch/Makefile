#########################################################################
# File Name: gen.sh
# Author: Gu Shenlong
# mail: blackhero98@gmail.com
# Created Time: Tue 10 May 2016 06:52:57 PM EDT
#########################################################################
#!/bin/bash
all:
	fly < test_dispatch.fly > client.cpp
	fly < test_server.fly > server.cpp
	g++ -std=c++11 -pthread -o server server.cpp
	g++ -std=c++11 -pthread -o client client.cpp
clean:
	rm -rf p1* p2*
	rm client* server* tmp.cc


