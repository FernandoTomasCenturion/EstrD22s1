#include <iostream>
using namespace std;
#include "Persona.h"

int main() {
  Persona carlos;                   // Preparame un espacio de memoria para una Persona
  carlos = nacer("Carlitos");       // Asigná el resultado de nacer a esta variable
  carlos = cumplirAnios(carlos);    // Asigná el resultado de cumplirAnios a esta variable
  ShowPersona(carlos);              // Hacé el efecto de ShowPersona con ese dato
  cout << edad(carlos) << endl;     // Mostrá el resultado de edad con ese dato
}
