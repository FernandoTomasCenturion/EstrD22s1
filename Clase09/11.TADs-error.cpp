#include <iostream>
using namespace std;
#include "Persona.h"

int main() {
  Persona carlos = nacer("Carlitos");
  Persona clonCarlos = cumplirAnios(carlos);
  ShowPersona(carlos);
  ShowPersona(clonCarlos);
}
