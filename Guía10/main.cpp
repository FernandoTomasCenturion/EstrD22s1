#include <iostream>
#include "Entrenador.h"

using namespace std;

int main() {
    Pokemon pok1 = consPokemon("Agua");
    Pokemon pok2 = consPokemon("Fuego");
    ShowPokemon(pok1); 
   
    cout << tipoDePokemon(pok1) << endl;
    cout << energia(pok1) << endl;   
    perderEnergia(50, pok1);
   
    cout << energia(pok1) << endl;
    cout << superaA(pok1, pok2) << endl;
    
    Pokemon* equipo = new Pokemon [1]; 
    equipo [0] = pok1;
    
    Entrenador e1 =   consEntrenador("ETomas", 20, equipo);
    return 0;
}