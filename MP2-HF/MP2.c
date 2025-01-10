#include <stdio.h>
#include <stdint.h>
#include "MP2.h"
#include <math.h>

double find_integral(int i, int j, int a, int b, int64_t n_integrals, int32_t* index, double* value) {
    for (int k = 0; k < n_integrals; k++) {
        if ((index[4 * k] == i && index[4 * k + 1] == j &&
             index[4 * k + 2] == a && index[4 * k + 3] == b) ||
            (index[4 * k] == i && index[4 * k + 1] == b &&
             index[4 * k + 2] == a && index[4 * k + 3] == j) ||
            (index[4 * k] == a && index[4 * k + 1] == b &&
             index[4 * k + 2] == i && index[4 * k + 3] == j) ||
            (index[4 * k] == a && index[4 * k + 1] == j &&
             index[4 * k + 2] == i && index[4 * k + 3] == b) ||
            (index[4 * k] == j && index[4 * k + 1] == i &&
             index[4 * k + 2] == b && index[4 * k + 3] == a) ||
            (index[4 * k] == b && index[4 * k + 1] == i &&
             index[4 * k + 2] == j && index[4 * k + 3] == a) ||
            (index[4 * k] == b && index[4 * k + 1] == a &&
             index[4 * k + 2] == j && index[4 * k + 3] == i) ||
            (index[4 * k] == j && index[4 * k + 1] == a &&
             index[4 * k + 2] == b && index[4 * k + 3] == i)) {
            return value[k];
        }
    }
    return 0.0;
}

double compute_mp2_energy(int32_t n_up, int32_t mo_num, double* mo_energy, 
                           int64_t n_integrals, int32_t* index, double* value) {
    double E_MP2 = 0.0;

    // Loop over occupied orbitals
    for (int i = 0; i < n_up; i++) {
        for (int j = 0; j < n_up; j++) {
            // Loop over virtual orbitals
            for (int a = n_up; a < mo_num; a++) {
                for (int b = n_up; b < mo_num; b++) {
                    // Calculate both integrals: ⟨ij | ab⟩ and ⟨ij | ba⟩
                    double integral_ijab = find_integral(i, j, a, b, n_integrals, index, value);  
                    double integral_ijba = find_integral(i, j, b, a, n_integrals, index, value);  // Swapping a and b

                    // Calculate the denominator using mo_energy[]
                    double denominator = mo_energy[i] + mo_energy[j] - mo_energy[a] - mo_energy[b];

                    // Handle small denominators to avoid division by zero
                    //if (fabs(denominator) < 1e-6) {
                    //    denominator = 1e-6;  // Regularize the denominator
                   // }

                    // Calculate the contribution to MP2 energy
                    double contribution = (integral_ijab * (2.0 * integral_ijab - integral_ijba)) / denominator;

                    // Accumulate the contribution to the MP2 energy
                    E_MP2 += contribution;
                }
            }
        }
    }

    // Print the final MP2 correlation energy
    //printf("MP2 Correlation Energy: %f\n", E_MP2);

    // Return the computed MP2 energy
    return E_MP2;
}

