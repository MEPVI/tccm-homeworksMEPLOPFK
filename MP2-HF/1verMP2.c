#include "MP2.h"
#include <stdio.h>
#include <stdint.h>

// Function to find the specific two-electron integral
double find_integral(int i, int j, int a, int b, int32_t* index, double* value, int64_t n_integrals) {
    for (int n = 0; n < n_integrals; n++) {
        if (index[4 * n] == i && index[4 * n + 1] == j &&
            index[4 * n + 2] == a && index[4 * n + 3] == b) {
            return value[n];
        }
    }
    return 0.0; // Return 0.0 if the integral is not found
}

// Compute MP2 energy
double compute_mp2_energy(int32_t n_up, int32_t mo_num, double* mo_energy,
                          int64_t n_integrals, int32_t* index, double* value) {
    double E_MP2 = 0.0;

    for (int i = 0; i < n_up; i++) { // Loop over occupied orbitals
        for (int j = 0; j < n_up; j++) {
            for (int a = n_up; a < mo_num; a++) { // Loop over virtual orbitals
                for (int b = n_up; b < mo_num; b++) {
                    double integral = find_integral(i, j, a, b, index, value, n_integrals);
                    double denominator = mo_energy[i] + mo_energy[j] - mo_energy[a] - mo_energy[b];
                    if (denominator != 0.0) {
                        E_MP2 += (integral * integral) / denominator;
                    } else {
                        fprintf(stderr, "Warning: Division by zero. i=%d, j=%d, a=%d, b=%d\n", i, j, a, b);
                    }
                }
            }
        }
    }

    printf("MP2 Energy: %lf\n", E_MP2);
    return E_MP2;
}
