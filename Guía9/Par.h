#include <iostream>
using namespace std;


struct RegistroDePar {
    int x;
    int y;
};


typedef struct RegistroDePar Par;

Par      consPar(int x, int y);
int      fstPar(Par p);
int      sndPar(Par p);
int      maxDelPar(Par p);
Par      swap(Par p);
Par      divisionYResto(int n, int m);
void     ShowPar(Par p);