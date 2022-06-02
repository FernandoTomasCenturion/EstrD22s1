#include <iostream>
using namespace std;

struct RegistroDeP {
  string nombre;
  int edad;
};

typedef struct RegistroDeP Persona;

Persona nacer(string n);
Persona cumplirAnios(Persona p);
string  nombre(Persona p);
int     edad(Persona p);
void    ShowPersona(Persona p);
