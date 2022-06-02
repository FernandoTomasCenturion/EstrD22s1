#include <iostream>
using namespace std;


struct RegistroDeFraccion {
    int numerador;
    int denominador;
}

typedef struct RegistroDeFraccion Fraccion;

Fraccion     consFraccion(int numerador, int denominador);
int          numerador(Fraccion f);
int          denominador(Fraccion f);
int          mcd(int x, int y;)
float        division(Fraccion f);
Fraccion     simplificada(Fraccion f);
Fraccion     sumF(Fraccion f1, Fraccion f2);
Fraccion     showFraccion(Fraccion f);

