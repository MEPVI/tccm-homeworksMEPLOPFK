#include <stdio.h>
#include <stdlib.h>
#include <trexio.h>

// Hartree-Fock energy calculation function declaration
double HF(int32_t N_occ, int32_t N_mo, double E_NN, double* const one_e_int, int64_t nInts, int32_t* index, double* const two_e_int);

int main() {
    trexio_exit_code rc;
    trexio_t* file = trexio_open("h2o.h5", 'r', TREXIO_AUTO, &rc);

    if (rc != TREXIO_SUCCESS) {
        printf("Error opening TREXIO file.\n");
        return 1;
    }

    // Declare variables
    double energy = 0.0;              // Nuclear repulsion energy
    int32_t n_up = 0;                 // Number of spin-up electrons
    int32_t mo_num = 0;               // Number of molecular orbitals
    double* mo_energy = NULL;         // Array of molecular orbital energies
    double* core_hamiltonian = NULL;  // Core Hamiltonian matrix
    int64_t n_integrals = 0;          // Number of two-electron integrals
    int32_t* index = NULL;            // Array for integral indices
    double* value = NULL;             // Array for integral values

    // Read nuclear repulsion energy
    if (trexio_exit_code trexio_read_nuclear_repulsion(file, &energy) != TREXIO_SUCCESS) {
        printf("Error reading nuclear repulsion energy.\n");
        trexio_close(file);
        return 1;
    }
    printf("Nuclear Repulsion Energy: %lf\n", energy);

    // Read the number of spin-up electrons
    if (trexio_read_electron_up_num(file, &n_up) != TREXIO_SUCCESS) {
        printf("Error reading number of spin-up electrons.\n");
        trexio_close(file);
        return 1;
    }
    printf("Number of spin-up electrons: %d\n", n_up);

    // Read molecular orbital data
    if (trexio_read_mo_data(file, &mo_num, &mo_energy) != TREXIO_SUCCESS) {
        printf("Error reading molecular orbital data.\n");
        trexio_close(file);
        return 1;
    }
    printf("Number of molecular orbitals: %d\n", mo_num);

    // Allocate memory for core Hamiltonian
    core_hamiltonian = malloc(mo_num * mo_num * sizeof(double));
    if (!core_hamiltonian) {
        printf("Memory allocation for core Hamiltonian failed.\n");
        trexio_close(file);
        return 1;
    }

    // Read one-electron integrals
    if (trexio_read_one_electron_integrals(file, mo_num, core_hamiltonian) != TREXIO_SUCCESS) {
        printf("Error reading one-electron integrals.\n");
        free(core_hamiltonian);
        trexio_close(file);
        return 1;
    }

    // Read two-electron integrals
    if (trexio_read_two_electron_integrals(file, &n_integrals, &index, &value) != TREXIO_SUCCESS) {
        printf("Error reading two-electron integrals.\n");
        free(core_hamiltonian);
        trexio_close(file);
        return 1;
    }

    // Calculate Hartree-Fock energy
    double E_HF = HF(n_up, mo_num, energy, core_hamiltonian, n_integrals, index, value);
    printf("Hartree-Fock Energy: %f\n", E_HF);

    // Free dynamically allocated memory
    free(core_hamiltonian);
    free(mo_energy);
    free(index);
    free(value);

    // Close the TREXIO file
    trexio_close(file);

    // Print the saved number of molecular orbitals
    printf("Saved number of molecular orbitals: %d\n", n_up);

    return 0;
}
