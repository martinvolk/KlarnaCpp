all:
	make libklarna
	make main
	
libklarna: libklarna.pyx
	cython libklarna.pyx
	
main: main.cpp
	g++ -I/usr/include/python2.7 -g -o main libklarna.c main.cpp -lpython2.7
