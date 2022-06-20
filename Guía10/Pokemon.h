#include <iostream>

using namespace std;

struct PokemonSt;

typedef PokemonSt* Pokemon; 

typedef string TipoDePokemon;

void ShowPokemon(Pokemon p);

Pokemon consPokemon(TipoDePokemon tipo); 

TipoDePokemon tipoDePokemon (Pokemon p);

int energia(Pokemon p);

void perderEnergia(int energia, Pokemon p); 

bool superaA(Pokemon p1, Pokemon p2); 