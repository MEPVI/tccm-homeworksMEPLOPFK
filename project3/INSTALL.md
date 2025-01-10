# Installation Instructions
Prerequisites
Before you begin, ensure you have the following software installed on your system:

- Fortran Compiler (e.g., gfortran) for MD project.
- TREXIO Library for MP2 project.
- GNU Make (optional but recommended).

## Installation Steps
### 1. Clone the Repository
Clone the project repository to your local system:
// git clone https://github.com/MEPVI/tccm-homeworksMEPLOPFK.git
// cd tccm-homeworksMEPLOPFK

### 2. Compile the Projects
For MD Project (Fortran)
1. Navigate to the MD project directory:
// cd project3
### 3. Compile the Fortran files:
// gfortran -o md_simulation MD_functions.f90
   
### 4. Running the Programs
MD Simulation
To run the MD simulation:
// ./md_simulation <input_file>
Replace <input_file> with the path to your molecular dynamics input file.

### 5. Testing the Programs
The repository includes a tests directory with input files for testing.
Run test files
// ./md_simulation tests/md_input.dat




