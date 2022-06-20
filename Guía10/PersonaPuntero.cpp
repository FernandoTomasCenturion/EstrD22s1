#include "PersonaPuntero.h"

struct PersonaSt {
    string nombre; 
    int edad; 
};

Persona consPersona(string nombre, int edad) { 
    PersonaSt* p = new PersonaSt;
    p -> nombre = nombre;
    p -> edad   = edad;
    return p;
 }

string nombre(Persona p) {
    return p -> nombre;
}

int edad(Persona p) { 
    return p -> edad;
}

void crecer(Persona p) {
     p -> edad ++; 
}

void cambioDeNombre(string nombre, Persona p) {
     p -> nombre = nombre; 
}

bool esMayorQueLaOtra(Persona p1, Persona p2) {
    return (p1 -> edad > p2 -> edad);
}

Persona laQueEsMayor(Persona p1, Persona p2) { 
    if (esMayorQueLaOtra(p1, p2)) {
        return p1; 
    } else { 
        return p2;
    }
}


void ShowPersona(Persona p) {
  cout << "Persona[" << p << "]("; 
  cout << "nombre <- \"" << p->nombre << "\", ";
  cout << "edad <- " << p->edad;
  cout << ")";
}

int main() {
    Persona persona1 = consPersona("Tomas", 24);
    Persona persona2 = consPersona("Nico", 25);
    ShowPersona(persona1);
    cout << nombre(persona1) << endl;
    cout << edad(persona1) << endl;
    crecer(persona1);
    cambioDeNombre("Juancito", persona1);
    cout << esMayorQueLaOtra(persona2, persona1) << endl;
    ShowPersona(laQueEsMayor(persona1, persona2));
    return 0;
}


