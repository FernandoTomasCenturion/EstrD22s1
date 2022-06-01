#include <iostream>
#include "Fracciones.h";
using namespace std;


void showFraccion (Fraccion f) {
    cout << f.numerador << endl;
    cout << "--" << endl;
    cout << f.denominador << endl;
}

Fraccion consFraccion(int numerador, int denominador) {
    Fraccion f; 
    f.numerador = numerador; 
    f.denominador= denominador;
    return f;
}

int numerador(Fraccion f) {
    return f.numerador;
}

int denominador(Fraccion f) {
    return f.denominador;
}

float division(Fraccion f) {
    return numerador(f) / denominador(f);
}

Fraccion multF(Fraccion f1, Fraccion f2){
    Fraccion newF; 
    newF.numerador = numerador(f1) * numerador(f2);
    newF.denominador = denominador(f1) * denominador(f2);
    return newF;
}


int mcd(int x, int y){
    int resultado;
    int n = max (x, y);
    int m = min (x, y);

    do{
        resultado = m;
        m = n % m;
        n = resultado;
    } while (m != 0);

    return resultado;
}


Fraccion simplificada(Fraccion f) {
    Fraccion resultado = f;
    int n = resultado.numerador;
    int d = resultado.denominador;
    resultado.numerador= resultado.numerador/(mcd(n,d));
    resultado.denominador= resultado.denominador/(mcd(n,d));
    return resultado;
}

  Fraccion sumF(Fraccion fraccion1, Fraccion fraccion2){
         return consFraccion(numerador(fraccion1), numerador(fraccion1));
    };




int main() {
    Fraccion f1 = consFraccion(4, 2);
    Fraccion f2 = consFraccion(1,2);
    cout << numerador(f1)  << endl;
    cout << denominador(f1)  << endl;
    cout << division(f1) << endl;
    showFraccion(f1);
    showFraccion (multF(f1, f2));
    showFraccion(simplificada(f1));
    showFraccion(sumF(f1,f2));
    return 0;
}