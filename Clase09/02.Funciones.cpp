#include <iostream>
using namespace std;

int succ(int n) { 
  return(n+1); 
}

int main() {
  int x = 17;
  int y = succ(x);
  cout << x << "+1=" << y << endl;
}


