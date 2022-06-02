#include <iostream>
using namespace std;
#include "Persona.h"

int main() {
  Persona carlos = nacer("Carlitos");
  cumplirAnios(carlos);
  ShowPersona(carlos);   // ¿Por qué imprime la edad en 0?
}
