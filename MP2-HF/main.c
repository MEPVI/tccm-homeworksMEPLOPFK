#include <stdio.h>
#include <stdlib.h>
#include <trexio.h>

void read_data_and_calculate(const char *filename) {
    trexio_t *trexio_file;
    trexio_exit_code rc;
    double energy, *mo_energy, *one_electron_integrals, *two_electron_values;
    int32_t n_up, mo_num;
    int64_t n_integrals;
    int32_t *two_electron_indices;

    // Open the TREXIO file
    trexio_file = trexio_open(filename, 'r', TREXIO_AUTO, &rc);
    if (rc != TREXIO_SUCCESS) {
        printf("TREXIO Error: %s\n", trexio_string_of_error(rc));
        exit(1);
    }

    // Read nuclear repulsion energy
    rc = trexio_read_nucleus_repulsion(trexio_file, &energy);
    if (rc != TREXIO_SUCCESS) {
        printf("Error reading nuclear repulsion energy: %s\n", trexio_string_of_error(rc));
        exit(1);
    }
    printf("Nuclear Repulsion Energy: %lf\n", energy);

    // Read number of up electrons
    rc = trexio_read_electron_up_num(trexio_file, &n_up);
    if (rc != TREXIO_SUCCESS) {
        printf("Error reading number of up electrons: %s\n", trexio_string_of_error(rc));
        exit(1);
    }
    printf("Number of up electrons: %d\n", n_up);

    // Read molecular orbital energies
    rc = trexio_read_mo_num(trexio_file, &mo_num);
    if (rc != TREXIO_SUCCESS) {
        printf("Error reading MO number: %s\n", trexio_string_of_error(rc));
        exit(1);
    }
    mo_energy = (double *)malloc(mo_num * sizeof(double));
    rc = trexio_read_mo_energy(trexio_file, mo_energy);
    if (rc != TREXIO_SUCCESS) {
        printf("Error reading MO energies: %s\n", trexio_string_of_error(rc));
        exit(1);
    }

    // Read one-electron integrals
    one_electron_integrals = (double *)malloc(mo_num * mo_num * sizeof(double));
    rc = trexio_read_mo_1e_int_core_hamiltonian(trexio_file, one_electron_integrals);
    if (rc != TREXIO_SUCCESS) {
        printf("Error reading one-electron integrals: %s\n", trexio_string_of_error(rc));
        exit(1);
    }

    // Read number of two-electron integrals
    rc = trexio_read_mo_2e_int_eri_size(trexio_file, &n_integrals);
    if (rc != TREXIO_SUCCESS) {
        printf("Error reading number of two-electron integrals: %s\n", trexio_string_of_error(rc));
        exit(1);
    }

    // Allocate memory for indices and values of two-electron integrals
    two_electron_indices = (int32_t *)malloc(4 * n_integrals * sizeof(int32_t));
    if (two_electron_indices == NULL) {
        fprintf(stderr, "Malloc failed for two-electron indices\n");
        exit(1);
    }
    two_electron_values = (double *)malloc(n_integrals * sizeof(double));
    if (two_electron_values == NULL) {
        fprintf(stderr, "Malloc failed for two-electron values\n");
        exit(1);
    }

    // Read two-electron integrals
    rc = trexio_read_mo_2e_int_eri(trexio_file, 0, &n_integrals, two_electron_indices, two_electron_values);
    if (rc != TREXIO_SUCCESS) {
        printf("Error reading two-electron integrals: %s\n", trexio_string_of_error(rc));
        exit(1);
    }

    // Close the TREXIO file
    rc = trexio_close(trexio_file);
    if (rc != TREXIO_SUCCESS) {
        printf("Error closing TREXIO file: %s\n", trexio_string_of_error(rc));
        exit(1);
    }

    // HF and MP2 calculations

    // Free allocated memory
 //   free(mo_energy);
  //  free(one_electron_integrals);
   // free(two_electron_indices);
   // free(two_electron_values);
//}
}
int main() {
    const char *filename = "./data/ch4.h5";
    read_data_and_calculate(filename);
    return 0;
}
