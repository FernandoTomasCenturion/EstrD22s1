#include <iostream>
using namespace std;

int main() {
  string msg = "Esto es un string largo";
  for(int i=11; i<17; i++) {
  	msg[i] = '*';
  }
  cout << msg << endl;
}
