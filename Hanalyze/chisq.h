#include <boost/math/distributions/chi_squared.hpp>
#include <iostream>

extern "C" {
void getPeripherals(double **, int, int, double**, double**, double*);
double getChiSq(double**, int, int, bool);
int test(int);
}
