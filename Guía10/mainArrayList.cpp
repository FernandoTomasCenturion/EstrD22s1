#include <iostream> 
#include "arrayList.h" 


int sumatoria(ArrayList xs) {
    int res=0
    for(int i =0; i< lenghtAL(xs); i++) {
        res= res + get(i, xs);
    }
    return res;
}

void sucesores ArrayList(xs) {
    for(int i = 0; i < lengthAL(xs); i++) {
        set(i, (get(i, xs) + 1) xs); 
    }
}

bool pertenece(int x, ArrayList xs) {
    bool res= false;
    int acc= 0;
    while(! res && acc < lenghtAl(xs)) {
        if(get(acc, xs) == x) {
            res=true;
        }
        else {
            acc ++;
        }
    }
    return res;
}


int apariciones (int x, ArrayList xs) {
    int cant=0;
    int acc= 0;
    while( acc < lenghtAl(xs)) {
        if(get(acc, xs) == x) {
            cant ++;
        }
        else {
            acc ++;
        }
    }
    return cant;
}


int main() {
    ArrayList arr1 = newArrayList(); 
    ArrayList arr2 = new ArrayListWith(32);
    return 0;

};