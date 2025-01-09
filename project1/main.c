#include <stdio.h>
#include <stdlib.h>
#include <trexio.h>

//declare function that are part of your codebase
void read_nuclear_repulsion(trexio_t*  file,const  double*  const  nuc_energy);
void read_electron_up_num(trexio_t* file, int32_t* n_up);
void read_mo_data(trexio_t* file, int32_t* mo_num, double** mo_energy);
void read_one_electron_integrals(trexio_t* file, int32_t mo_num, double* core_hamiltonian);
void read_two_electron_integrals(trexio_t* file, int64_t* n_integrals, int32_t** index, double** value);
double HF(int32_t N_occ, int32_t N_mo, double E_NN, double* const one_e_int, int64_t nInts, int32_t* index, double* const two_e_int);


int main() {
    trexio_exit_code rc;
    trexio_t* file = trexio_open("h2o.h5", 'r', TREXIO_AUTO, &rc);

    if (rc != TREXIO_SUCCESS) {
        printf("Error opening file.\n");
        return 1;
    }

    // declare variable for storing nuc energy
     double  nuc_energy=0.0;
    read_nuclear_repulsion(file, &nuc_energy);
    printf("Nuclear Repulsion Energy from function: %lf\n", nuc_energy);

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
    read_one_electron_integrals(file, mo_num, core_hamiltonian);
    //free(core_hamiltonian);

    int64_t n_integrals;
    int32_t* index;
    double* value;
    read_two_electron_integrals(file, &n_integrals, &index, &value);
    double E_HF = HF(n_up, mo_num, nuc_energy, core_hamiltonian, n_integrals, index, value);
    printf("HF Energy: %f", E_HF);

    free(index);
    free(value);

    free(core_hamiltonian);
    free(mo_energy);
    trexio_close(file);

    // Optionally print the saved number of molecular orbitals for verification
    printf("Saved number of molecular orbitals: %d\n", saved_mo_num);

    return 0;
}
