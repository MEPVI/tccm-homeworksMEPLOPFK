#include <stdio.h>
#include <stdlib.h>
#include <trexio.h>




int main() {
    //Variable to store the return code from TREXIO functions.
    trexio_exit_code rc;

    //Open TREXIO file in read mode with an automatic backend detection
    trexio_t* file = trexio_open("c2h2.h5", 'r', TREXIO_AUTO, &rc);
    // Check if the file was opened successfully
    if (rc != TREXIO_SUCCESS) {
        printf("Error opening file.\n");
        return 1; //Exit the program with an error code if the file cannot be opened
    }
// Read the nuclear repulsion energy from the TREXIO file
    read_nuclear_repulsion(file);
    read_electron_up_num(file);
// Declare variables to store molecular orbital (MO) data
    int32_t mo_num;   // Number of molecular orbitals
    double* mo_energy; // Array to store MO energies
    
    //Read the number of MOs and their energies
    read_mo_data(file, &mo_num, &mo_energy);
    //saved the number of MOs in a variable
    int32_t saved_mo = mo_num;
    // allocate memory for core Hamiltonian matrix 
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

//exit

    return 0;
}
