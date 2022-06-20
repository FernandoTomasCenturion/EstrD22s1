#include <iostream>
#include "arrayList.h"


struct ArrayListSt {
int cantidad; // cantidad de elementos
int* elementos; // array de elementos
int capacidad; // tamaño del array
}


ArrayList newArrayList() {
    ArrayList arr = new ArrayList; 
    arr -> cantidad = 0;
    arr -> elementos = new int [16];
    arr -> capacidad = 16;   
}


ArrayList newArrayListWith(int capacidad) {
    ArrayList arr = new ArrayList; 
    arr -> cantidad = 0;
    arr -> elementos = new int [capacidad];
    arr -> capacidad = capacidad;   
}

int lenghtAL(ArrayList xs) { 
    return xs -> cantidad; 
}

int get(int i, ArrayList xs) {
    return xs -> elementos [i];
}

void set(int i, int x, ArrayList xs) {
    return xs -> elementos [i] = x;
}

void resize(int capacidad, ArrayList xs) {
    int* arrayViejo = xs -> elementos;
    int cantidadViejo= xs -> cantidad;
    xs -> cantidad=0;

    if(capacidad > xs-> cantidad) {
        xs -> elementos = new int [capacidad]; 
                 if(cantidadAntiguo > capacidad){
                            for(int i = 0; i < capacidad; i++){
                                xs->elementos[i] = arrayAntiguo[i];
                                xs->cantidad++;
                            }
                        
                        }
                        else{
                            for(int i = 0; i < cantidadAntiguo ; i++){
                                xs->elementos[i] = arrayAntiguo[i];
                                xs->cantidad = cantidadAntiguo;
                            }
                        }

    }
    else{
        xs->elementos = new int[capacidad];
        for(int i = 0; i < xs->cantidad; i++){
            xs->elementos[i] = arrayAntiguo[i];
        }
        
    }
    xs->capacidad = capacidad;
    delete arrayAntiguo;
}

void add(int x, ArrayList xs) {
    if(xs-> cantidad > xs -> capacidad) {
        xs-> elementos [xs-> cantidad] = x; 
        xs-> cantidad ++;
    }
}

void remove(ArrayList xs) {
    int nuevaCantidad = (xs-> cantidad) - 1; 
    int* antiguoArray = xs-> elementos; 
    if(xs -> cantidad >0) { 
        xs -> elementos = new int [xs -> capacidad]; 
        for(int i=0; i < nuevaCantidad; i++) {
            xs-> elementos [i] = antiguoArray [i]; 
        }
        xs-> cantidad --; 
    }
    delete antiguoArray;
}