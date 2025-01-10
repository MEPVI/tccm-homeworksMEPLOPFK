program MD_program
    USE MD_functions
    IMPLICIT NONE

    INTEGER :: Natoms, i, j, n_step
    DOUBLE PRECISION, ALLOCATABLE :: coord(:, :), mass(:), distance(:, :)
    CHARACTER(len=100) :: input_file
    DOUBLE PRECISION :: epsilon, sigma, V_tot
    DOUBLE PRECISION, ALLOCATABLE :: velocity(:, :), acceleration(:,:)
    DOUBLE PRECISION :: total_kinetic_energy
    REAL :: timestep

    ! Initialize parameters
    input_file = "inp.txt"   ! Replace with your actual input file
    epsilon = 0.0661                 ! Example Lennard-Jones epsilon parameter
    sigma = 0.3345                    ! Example Lennard-Jones sigma parameter
    n_step= 1000   !integers 
    timestep= 0.2  ! real
   
 
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
     
! call acce
    CALL compute_acc(Natoms, coord, mass, distance, acceleration, sigma, epsilon)

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
     PRINT *, "position:", coord(1,:)
end do   


    ! Deallocate arrays
    DEALLOCATE(coord, mass, distance, velocity)
END PROGRAM MD_program
