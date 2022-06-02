#include <iostream>
using namespace std;

struct Persona {
  string nombre;
  int edad;
}; 

int main() {
  struct Persona p;
  p.nombre = "Alejandro";
  p.edad = 36;
  cout << p.nombre << endl;
}
