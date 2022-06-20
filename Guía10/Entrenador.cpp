#include <iostream>
#include "Entrenador.h"

struct EntrenadorSt {
    string nombre;
    Pokemon* pokemones;
    int cantPokemon;
};

Entrenador consEntrenador(string nombre, int cantidad, Pokemon* pokemones) {
    EntrenadorSt* e = new EntrenadorSt; 
    e -> nombre = nombre;
    e -> pokemon = pokemones; 
    e -> cantPokemon= cantidad;
    return e;
}

string nombreDeEntrenador(Entrenador e) {
    return e -> nombre; 
}

int cantidadDePokemon (Entrenador e) {
    return e -> cantPokemon; 
}

int cantidadDePokemonDe(TipoDePokemon tipo, Entrenador e) {
    int res = 0
    for(int i = 0; i<e e -> cantidad; i++) {
        if (tipoDePokemon(e-> pokemones[i]) == tipo) { 
            res ++;
        }
    }
    return res;
}

Pokemon pokemonNro(int i, Entrenador e) { 
    return e -> pokemones [i];
}