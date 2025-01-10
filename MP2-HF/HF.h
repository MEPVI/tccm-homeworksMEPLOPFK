// HF.h
//double HF(int32_t N_occ, int32_t N_mo, double E_NN, double* const one_e_int, int64_t nInts, int32_t* index, double* const two_e_int);
//A function to obtain the HF energy
#ifndef HF_ENERGY_H
#define HF_ENERGY_H

#include <stdint.h>

double HF(int32_t N_occ, int32_t N_mo, double E_NN, double* const one_e_int,
          int64_t nInts, int32_t* index, double* const two_e_int);

#endif

