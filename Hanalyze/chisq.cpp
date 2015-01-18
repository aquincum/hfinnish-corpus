#include <boost/math/distributions/chi_squared.hpp>
#include <iostream>
#include "chisq.h"

using namespace boost::math;
using namespace std;

void getPeripherals(double **table, int x, int y, double **rowsums, double **colsums, double *grandtotal){
  int i = 0, j = 0;
  *rowsums = new double[x];
  *colsums = new double[y];
  *grandtotal = 0.0;
  for(j = 0; j < y; j ++)
	(*colsums)[j] = 0.0;
  for (i = 0; i < x; i ++){
	(*rowsums)[i] = 0.0;
	for ( j = 0; j < y; j ++){
	  (*rowsums)[i] += table[i][j];
	  (*colsums)[j] += table[i][j];
	}
	*grandtotal += (*rowsums)[i];
  }
}

int test(int x){
  return x + 1;
}


double getChiSq(double **table, int x, int y, bool yates = false){
  double *rowsums, *colsums, grandtotal, chisq = 0.0;
  getPeripherals(table, x, y, &rowsums, &colsums, &grandtotal);
  for (int i = 0; i < x; i ++){
	for (int j = 0; j < y; j ++){
	  double expected = (rowsums[i] / grandtotal) * (colsums[j] / grandtotal) * grandtotal;
	  double oe = abs(table[i][j] - expected) - (yates ? 0.5 : 0);
	  chisq += (oe * oe) / expected;
	}
  }
  delete rowsums;
  delete colsums;
  return chisq;
}


int exmain (){
  chi_squared dist(1.0);
  double **testdata = new double*[2];
  testdata[0] = new double[2];
  testdata[1] = new double[2];
  testdata[0][0] = 12.0;
  testdata[0][1] = 7.0;
  testdata[1][0] = 5.0;
  testdata[1][1] = 7.0;

  double testchsq = getChiSq(testdata, 2, 2);
  cout << "\n" << testdata[1][1] << "\n" ;
  double testchsqyat = getChiSq(testdata, 2, 2, true);
  double x = getChiSq(testdata,2,2);
  x = getChiSq(testdata,2,2);
  x = getChiSq(testdata,2,2);
  x = getChiSq(testdata,2,2);
  double p_lower = quantile(dist,0.025);
  double p_upper = quantile(dist,0.975);
  double p = cdf(dist, testchsq);
  double pyat = cdf(dist, testchsqyat);

  cout << "For the test data:\n";
  cout << "chi_sq = " << testchsq << "\n";
  cout << "chi_sq(yates) = " << testchsqyat << "\n";
  cout << "p(lower) = " << p_lower << "\n";
  cout << "p(upper) = " << p_upper << "\n";
  cout << "p = " << p << "\n";
  cout << "p(yates) = " << pyat << "\n";
  return 0;
}
