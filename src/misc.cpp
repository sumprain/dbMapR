#include <Rcpp.h>
using namespace Rcpp;

//' Get next number for PK generation
//' In a vector of numbers, the function finds out the first missing number and returns it.
//' If it does not find any missing value, it return number next to the last number.
//' @param x vector of numbers.
// [[Rcpp::export]]
int nextNumber(IntegerVector x) {

  /*
   * The function will be used to get next PK id for a table.
   * Made by suman on 19 May 2015.
  */

  int n = x.size();

  if (n == 0)
    return 1;

  x = x.sort();
  int i = 0;
  int expected = 1;

  if (x[i] == 0)
    stop("first number should be 1.");

  if (any(is_na(x)) == TRUE)
    stop("missing value is not allowed.");

  if (unique(x).size() != n)
    stop("duplicate numbers not allowed.");

  while(x[i] == expected) {
    i = i + 1;
    expected = expected + 1;

    if (i == n)
      break;
  }

  return expected;

}
