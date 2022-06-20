#include <iostream>
using namespace std;



typedef ArrayListSt* ArrayList;

struct ArrayListSt {
int cantidad; // cantidad de elementos
int* elementos; // array de elementos
int capacidad; // tamaño del array
}


ArrayList newArrayList();
ArrayList newArrayListWith(int capacidad);
int lengthAL(ArrayList xs); 
int get(int i, ArrayList xs);
void set(int i, int x, ArrayList xs);
void add(int x, ArrayList xs);
void remove(ArrayList xs);
