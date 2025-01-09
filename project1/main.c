#include <stdio.h>
#include <stdlib.h>
#include <trexio.h>

//declaration of functions to read specific data from TREXIO Library 
void read_nuclear_repulsion(trexio_t* file);
void read_electron_up_num(trexio_t* file, int32_t* n_up);
void read_mo_data(trexio_t* file, int32_t* mo_num, double** mo_energy);
void read_one_electron_integrals(trexio_t* file, int32_t mo_num, double* core_hamiltonian);
void read_two_electron_integrals(trexio_t* file, int64_t* n_integrals, int32_t** index, double** value);

int main() {
    //Variable to store the return code from TREXIO functions.
    trexio_exit_code rc;
    trexio_t* file = trexio_open("ch4.h5", 'r', TREXIO_AUTO, &rc);

    if (rc != TREXIO_SUCCESS) {
        printf("Error opening file.\n");
        return 1; //Exit the program with an error code if the file cannot be opened
    }
// Read the nuclear repulsion energy from the TREXIO file
    read_nuclear_repulsion(file);
    // Declare variable to store the number of spin-up electrons
    int32_t n_up;
    // read the number of spin-up electrons 
    read_electron_up_num(file, &n_up);

    int32_t mo_num;
    double* mo_energy;
    read_mo_data(file, &mo_num, &mo_energy);
// occupied number of orbitals
   int32_t saved_mo_num = n_up;
    // Save the number of molecular orbitals into a separate variable
   // int32_t saved_mo_num = mo_num;

    double* core_hamiltonian = malloc(mo_num * mo_num * sizeof(double));
    // Read one-electron integrals (core Hamiltonian) from TREXIO
    read_one_electron_integrals(file, mo_num, core_hamiltonian);
    // Free the memory allocated for the core hamiltonian
    free(core_hamiltonian);
// Declaring variables to store two-electron integral data
    int64_t n_integrals; //Number of non-zero two-electron integrals
    int32_t* index;     // Array to store indices of the integrals
    double* value;     // Array to store values of the integrals
    // Read two-electron integrals from the TREXIO file
    read_two_electron_integrals(file, &n_integrals, &index, &value);
    // Free the memory allocated for the two-electron integrals
    free(index);
    free(value);
    // Free the memory allocated for molecular orbital energies
    free(mo_energy);
// Close the TREXIO file to release resources
    trexio_close(file);

    // Optionally print the saved number of molecular orbitals for verification
    printf("Saved number of molecular orbitals: %d\n", saved_mo_num);

    return 0;
}
