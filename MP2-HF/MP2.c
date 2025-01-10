#include <stdio.h>
<<<<<<< HEAD
#include <stdlib.h>
#include <stdint.h>
#include <trexio.h>

// Function to compute MP2 energy correction
double compute_mp2_energy(trexio_t *trexio_file) {
    int32_t n_mo, n_occ;
    trexio_read_mo_num(trexio_file, &n_mo);
    trexio_read_electron_up_num(trexio_file, &n_occ);

    double *mo_energies = malloc(n_mo * sizeof(double));
    trexio_read_mo_energy(trexio_file, mo_energies);

    int64_t n_integrals;
    trexio_read_mo_2e_int_eri_size(trexio_file, &n_integrals);

    int32_t *indices = malloc(4 * n_integrals * sizeof(int32_t));
    double *values = malloc(n_integrals * sizeof(double));
    int64_t buffer_size = n_integrals;

    trexio_read_mo_2e_int_eri(trexio_file, 0, &buffer_size, indices, values);

    double emp2 = 0.0;
    int count = 0;

    for (int64_t idx = 0; idx < n_integrals; idx++) {
        int i = indices[4 * idx + 0];
        int j = indices[4 * idx + 1];
        int a = indices[4 * idx + 2];
        int b = indices[4 * idx + 3];
        double integral_value = values[idx];

        if (integral_value < 1e-10) continue; // Skip near-zero integrals

        if (i < n_occ && j < n_occ && a >= n_occ && b >= n_occ) {
            double numerator = integral_value * (2 * integral_value - integral_value);
            double denominator = mo_energies[i] + mo_energies[j] - mo_energies[a] - mo_energies[b];

            if (denominator != 0.0) {
                double contribution = numerator / denominator;
                emp2 += contribution;

                // Debug: Log contributions
                printf("Contribution[%lld]: Indices(%d, %d, %d, %d) Value: %.8f Contribution: %.8f\n",
                       (long long)idx, i, j, a, b, integral_value, contribution);
                count++;
=======
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
>>>>>>> c2b05df410a4c767a7aa6d62f084ed4568019576
            }
        }
    }

<<<<<<< HEAD
    printf("Total Contributions: %d\n", count);

    free(mo_energies);
    free(indices);
    free(values);

    return emp2;
}

// Main function
int main(int argc, char **argv) {
    if (argc < 2) {
        printf("Usage: %s <trexio_file>\n", argv[0]);
        return 1;
    }

    trexio_exit_code rc;
    trexio_t *trexio_file = trexio_open(argv[1], 'r', TREXIO_AUTO, &rc);

    if (rc != TREXIO_SUCCESS) {
        printf("TREXIO Error: %s\n", trexio_string_of_error(rc));
        return 1;
    }

    double mp2_energy = compute_mp2_energy(trexio_file);
    printf("MP2 Energy Correction: %.8f a.u.\n", mp2_energy);

    rc = trexio_close(trexio_file);
    if (rc != TREXIO_SUCCESS) {
        printf("TREXIO Error closing file: %s\n", trexio_string_of_error(rc));
        return 1;
    }

    return 0;
}
=======
    // Print the final MP2 correlation energy
    //printf("MP2 Correlation Energy: %f\n", E_MP2);

    // Return the computed MP2 energy
    return E_MP2;
}

>>>>>>> c2b05df410a4c767a7aa6d62f084ed4568019576
