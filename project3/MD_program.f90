program MD_program
    USE MD_functions
    IMPLICIT NONE

    INTEGER :: Natoms, i, j, n_step, Msteps
    DOUBLE PRECISION, ALLOCATABLE :: coord(:, :), mass(:), distance(:, :)
    CHARACTER(len=100) :: input_file, output_file
    DOUBLE PRECISION :: epsilon, sigma, V_tot, E_tot
    DOUBLE PRECISION, ALLOCATABLE :: velocity(:, :), acceleration(:,:)
    DOUBLE PRECISION :: total_kinetic_energy
    REAL :: timestep
    

     
    ! Initialize parameters
    input_file = "inp.txt"   ! Replace with your actual input file
    output_file= "output.out"  ! output file
    epsilon = 0.0661                 ! Example Lennard-Jones epsilon parameter
    sigma = 0.3345                    ! Example Lennard-Jones sigma parameter
    n_step= 1000   !integers 
    timestep= 0.2  ! real
    Msteps= 200 !Print every Msteps
   
 
    ! Read the number of atoms
    Natoms = read_Natoms(input_file)
    PRINT *, "Number of atoms:", Natoms

    ! Allocate arrays based on number of atoms
    ALLOCATE(coord(Natoms, 3), mass(Natoms), distance(Natoms, Natoms), &
    velocity(Natoms, 3), acceleration(Natoms, 3))

    ! Read molecule data
    CALL read_molecule(input_file, Natoms, coord, mass)

    ! Compute distances
    CALL compute_distances(Natoms, coord, distance)


    ! Compute potential energy
    V_tot = V(epsilon, sigma, Natoms, distance)
    PRINT *, "Potential energy:", V_tot

    ! Example: Initialize velocities and compute kinetic energy
    velocity = 0.00  ! Replace with actual velocity initialization
    total_kinetic_energy = T(Natoms, velocity, mass)
    PRINT *, "Total kinetic energy:", total_kinetic_energy
    !PRINT *, "vel", velocity(3,2)    
     
    E_tot = total_kinetic_energy + V_tot  
! call acce
    CALL compute_acc(Natoms, coord, mass, distance, acceleration, sigma, epsilon)

    !Printing in output file the initial steps
    OPEN(unit=10, file=output_file, status='replace', action='write')
    WRITE(10,*) Natoms
    WRITE(10,*) "# E_K:", total_kinetic_energy, "E_V:", V_tot, "E_tot:", E_tot   
    DO i=1,Natoms
    WRITE(10,*) coord(i,:)
 END DO


     !start MD
     do i = 1, n_step
          !update r
          CALL update_r(Natoms, coord, velocity, acceleration, timestep)

          !update v without updated a
          CALL update_v(Natoms, coord, velocity, acceleration, timestep)
          
          !update acceleration
          CALL update_v(Natoms, coord, velocity, acceleration, timestep)

          !update v with updated a
          CALL update_v(Natoms, coord, velocity, acceleration, timestep)
            !printing output
            if(mod(i,Msteps)== 0) then 
             total_kinetic_energy = T(Natoms, velocity, mass)
               CALL compute_distances(Natoms, coord, distance)
              V_tot = V(epsilon, sigma, Natoms, distance)
              E_tot = total_kinetic_energy + V_tot 
              !Printing in output file the initial steps
    WRITE(10,*) Natoms
    WRITE(10,*) "# E_K:", total_kinetic_energy, "E_V:", V_tot, "E_tot:", E_tot
    DO j=1,Natoms
    WRITE(10,*) coord(j,:)
 END DO

             endif
end do   
 CLOSE(10)

    ! Deallocate arrays
    DEALLOCATE(coord, mass, distance, velocity, acceleration)



END PROGRAM MD_program
