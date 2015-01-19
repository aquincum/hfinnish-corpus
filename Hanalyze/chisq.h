#include <boost/math/distributions/chi_squared.hpp>
#include <iostream>
#define EXPORT __attribute__ ((visibility ("default")))

extern "C" {
  EXPORT void getPeripherals(double **, int, int, double**, double**, double*);
  EXPORT double getChiSq(double**, int, int, int);
  EXPORT int test(int);
}
