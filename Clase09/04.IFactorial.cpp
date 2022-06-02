#include <iostream>
using namespace std;

int ifact(int n) { 
  int f = 1;
  int current = n;
  while (current>0)
   { f = f*current; current = current-1; }
  return(f);
}

int main() {
  int x = 4;
  int y = ifact(x);
  cout << "fact(" << x << ")=";
  cout << y << endl;
}
