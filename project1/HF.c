#include <stdint.h>
#include <assert.h>

// C function to calculate the Hartree-Fock (HF) energy
double HF(int32_t* N_occ, int32_t* N_mo, double E_NN, double* const one_e_int, 
          int64_t nInts, int32_t* index, double* const two_e_int) {
    
    // Dereference N_occ and N_mo for easier usage
    int N_occ_val = *N_occ;
    int N_mo_val = *N_mo;

    // Initialize one- and two-electron summations to zero
    double one_e_sum = 0;
    double two_e_sum = 0;

    // Add iteratively the one-electron terms
    for (int i = 0; i < N_occ_val; i++) {
        // Add new term to the one-electron summation
        one_e_sum += one_e_int[i * N_mo_val + i]; // Reading diagonal of array
    }

    // Add iteratively the two-electron terms
    for (int64_t n = 0; n < nInts; n++) { // Iterate over integral array once
        // Define indices for the integral "n"
        int i = index[4 * n];
        int j = index[4 * n + 1];
        int k = index[4 * n + 2];
        int l = index[4 * n + 3];

        // Only use if all indices belong to occupied orbitals
        if (i < N_occ_val && j < N_occ_val && k < N_occ_val && l < N_occ_val) {
            if (i == j && j == k && k == l) {                // Case 1: All indices equal
                two_e_sum += two_e_int[n];
            } else if (i == k && j == l) {                   // Case 2: Coulomb integral
                two_e_sum += 4 * two_e_int[n];
            } else if (i == j && k == l) {                   // Case 3: Exchange integral
                two_e_sum -= 2 * two_e_int[n];
            }
        }
    }

    // Calculate the HF energy
    double E_HF = E_NN + 2 * one_e_sum + two_e_sum;

    return E_HF;
}
