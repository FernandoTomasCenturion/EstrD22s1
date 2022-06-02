#include <iostream>
#include "Persona.h"
using namespace std;

Persona nacer(string n) {
  Persona p;
  p.nombre = n; p.edad = 0;
  return(p);
}

Persona cumplirAnios(Persona p) {
  p.edad++;
  return(p);
}

string nombre(Persona p) {
  return(p.nombre);
}

int edad(Persona p) {
  return(p.edad);
}

void ShowPersona(Persona p) {
  cout << "Persona("; 
  cout << "nombre <- \"" << p.nombre << "\", ";
  cout << "edad <- " << p.edad;
  cout << ")" << endl;
}
