#ifndef MP2_H
#define MP2_H

#include <stdint.h>

// Function to find a specific two-electron integral
double find_integral(int i, int j, int a, int b, int32_t* index, double* value, int64_t n_integrals);

// Function to compute the MP2 energy
double compute_mp2_energy(int32_t n_up, int32_t mo_num, double* mo_energy,
                          int64_t n_integrals, int32_t* index, double* value);

#endif // MP2_H
