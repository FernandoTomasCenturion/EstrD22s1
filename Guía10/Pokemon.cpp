#include "Pokemon.h"

struct PokemonSt {
    TipoDePokemon tipo;
    int vida;
};

Pokemon consPokemon(TipoDePokemon tipo){
    PokemonSt* p = new PokemonSt;
    p->tipo = tipo;
    p->vida = 100;
    return p;
}

TipoDePokemon tipoDePokemon(Pokemon p) {
    return p -> tipo;
}

int energia(Pokemon p) {
    return p -> vida;
}

void perderEnergia(int energia, Pokemon p) {
    p -> vida -= energia;
}

bool esTipoSuperior(TipoDePokemon t1, TipoDePokemon t2) {
    return (t1 =="Agua"    && t2 == "Fuego") || 
            (t1 == "Fuego"  && t2 == "Planta") ||
            (t1 == "Planta" && t2 == "Agua");
}

bool superaA(Pokemon p1, Pokemon p2) {
    return esTipoSuperior (p1-> tipo, p2-> tipo) ;
}


void ShowPokemon(Pokemon p) {
  cout << "Pokemon[" << p << "]("; 
  cout << "tipo <- \"" << p->tipo << "\", ";
  cout << "vida <- " << p->vida;
  cout << ")";
}
