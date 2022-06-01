#include <iostream>
#include "Par.h"
using namespace std;


Par consPar(int x, int y) {
    Par p; 
    p.x= x; p.y= y;
    return p;
}

int fstPar(Par p) {
    return p.x;
}

int sndPar(Par p){
    return p.y;
}

int maxDelPar(Par p){
    return max(p.x, p.y);
}

Par swap(Par p) {
   return consPar (p.y, p.x);
}

void showPar (Par p) {
    cout << "Par (" << p.x  <<  ", "<<  p.y  << ")" << endl;
}

Par divisionYResto(int n, int m) {
    Par newP = consPar(n,m);
    newP.x = newP.x / newP.y;
    newP.y = newP.x % newP.y;

    return newP;
}



int main() {
    Par p1 = consPar(8,9);
    Par p2 = divisionYResto(4,2);  
    cout <<  fstPar(p1)    << endl;
    cout <<  sndPar(p1)    << endl;
    cout <<  maxDelPar(p1) << endl;
    showPar(p1);
    showPar(swap(p1));
    showPar(p2);
    return 0;
}