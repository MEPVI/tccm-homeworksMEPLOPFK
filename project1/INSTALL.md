# Installation Instructions
Prerequisites
Before you begin, ensure you have the following software installed on your system:

- C Compiler (e.g., gcc) for MP2 project.
- TREXIO Library for MP2 project.
- GNU Make (optional but recommended).

## Installation Steps
### 1. Clone the Repository
Clone the project repository to your local system:
// git clone https://github.com/MEPVI/tccm-homeworksMEPLOPFK.git
// cd tccm-homeworksMEPLOPFK

### 2. Compile the Projects
For HF/MP2 Project (Fortran)
1. Navigate to the HF-MP2 project directory:
// cd project
2. Compile the Fortran files:
// gfortran -o md_simulation MD_functions.f90
4. For MP2 Project (C)
Navigate to the MP2 project directory:
// cd MP2-HF
2. Make sure the TREXIO Library is correctly installed and paths are configured:
Include paths for the TREXIO library.
Example for macOS/Linux:
// gcc -I/path/to/trexio/include \
    -L/path/to/trexio/src/ -ltrexio \
    main.c HF.c MP2.c -o main
   
4. Running the Programs
MD Simulation
To run the MD simulation:
// ./md_simulation <input_file>
Replace <input_file> with the path to your molecular dynamics input file.
MP2 Energy Calculations
To run the MP2 program:
// ./main
4. Testing the Programs
The repository includes a tests directory with input files for testing.

For MD:
// ./md_simulation tests/md_input.dat
For MP2:
// ./main tests/mp2_input.h5
