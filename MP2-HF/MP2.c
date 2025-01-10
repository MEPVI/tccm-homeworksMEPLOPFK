#include <stdio.h>
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
            }
        }
    }

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
