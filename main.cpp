#include "Python.h"
#include <stdio.h>

extern "C"{
void test();
void initinterface();
void initlibklarna();
void Py_Initialize();
void Py_Finalize();
int Klarna_Init(int, char*, char*);
}

int main(){
	Py_Initialize();
	initlibklarna();
	test();
	try{
		Klarna_Init(12345, "secret", "beta");
		/*Klarna_AddOrderLine(3, "123123", "Test Product!", 5.50, 25, 0);
		Klarna_Address(
			"foo@bar.com",
			"0123123",
			"0773000000",
			"Testperson-se",
			"Approved",
			" ",
			"Some street",
			"12345",
			"City",
			"Country",
			"11");
		Klarna_SetOrderInfo("23423", "234234", "user", "A comment!");
		Klarna_Submit("410321-9202");*/
	} catch(...){
		printf("AN ERROR OCCURED!");
		if(PyErr_Occurred()){
			PyErr_Print();
		}
	}
	Py_Finalize();
	return 0;
}
