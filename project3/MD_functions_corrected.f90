
MODULE MD_functions
    IMPLICIT NONE

    CONTAINS

    INTEGER FUNCTION read_Natoms(input_file) RESULT(Natoms)
        IMPLICIT NONE
        CHARACTER(len=*), INTENT(IN) :: input_file

        ! Open input file in read mode
        OPEN(unit=1, file=input_file, action='read', status='old', form='formatted')
        READ(1, *) Natoms ! Read first line, and store number of atoms in Natoms
        CLOSE(unit=1) ! Close file after reading
    END FUNCTION read_Natoms

    SUBROUTINE read_molecule(input_file, Natoms, coord, mass)
        IMPLICIT NONE
        CHARACTER(len=*), INTENT(IN) :: input_file
        INTEGER, INTENT(IN) :: Natoms
        DOUBLE PRECISION, INTENT(OUT) :: coord(Natoms, 3)
        DOUBLE PRECISION, INTENT(OUT) :: mass(Natoms)
        INTEGER :: i

        ! Open input file in read mode
        OPEN(unit=1, file=input_file, action='read', status='old', form='formatted')
        READ(1, *) ! Read first line to skip it

        ! Iterate over the lines of the file and store the coordinates and masses
        DO i = 1, Natoms
            READ(1, *) coord(i, 1), coord(i, 2), coord(i, 3), mass(i)
        END DO
        CLOSE(unit=1)
    END SUBROUTINE read_molecule
! Subroutine to get a distance matrix between atoms
    SUBROUTINE compute_distances(Natoms, coord, distance)
        IMPLICIT NONE
        INTEGER, INTENT(IN) :: Natoms
        DOUBLE PRECISION, INTENT(IN) :: coord(Natoms, 3)
        DOUBLE PRECISION, INTENT(OUT) :: distance(Natoms, Natoms)
        INTEGER :: i, j

        DO i = 1, Natoms
            DO j = 1, Natoms
                distance(i, j) = SQRT((coord(i, 1) - coord(j, 1))**2 &
                                     + (coord(i, 2) - coord(j, 2))**2 &
                                     + (coord(i, 3) - coord(j, 3))**2)
            END DO
        END DO
    END SUBROUTINE compute_distances        
 

DOUBLE PRECISION FUNCTION V(epsilon, sigma, Natoms, distance) RESULT(V_tot)
    IMPLICIT NONE
    DOUBLE PRECISION, INTENT(IN) :: epsilon, sigma
    INTEGER, INTENT(IN) :: Natoms
    DOUBLE PRECISION, INTENT(IN) :: distance(Natoms, Natoms)
    INTEGER :: i, j

    ! Initialize V_tot to zero
    V_tot = 0.0d0

    ! Iterate over all pairs of atoms and calculate contributions to V_tot
    DO i = 1, Natoms
        DO j = i + 1, Natoms  ! Only iterate over j > i to avoid duplicates
            IF (distance(i, j) > 0.0d0) THEN
                V_tot = V_tot + 4 * epsilon * ((sigma / distance(i, j))**12 - (sigma / distance(i, j))**6)
            END IF
        END DO
    END DO

END FUNCTION V


    FUNCTION T(Natoms, velocity, mass) RESULT(total_kinetic_energy)
        IMPLICIT NONE
        INTEGER, INTENT(IN) :: Natoms
        DOUBLE PRECISION, INTENT(IN) :: velocity(Natoms, 3), mass(Natoms)
        DOUBLE PRECISION :: total_kinetic_energy
        INTEGER :: i, j
        DOUBLE PRECISION :: velocity_squared

        total_kinetic_energy = 0.0d0
        DO i = 1, Natoms
            velocity_squared = 0.0d0
            DO j = 1, 3
                velocity_squared = velocity_squared + velocity(i, j)**2
            END DO
            total_kinetic_energy = total_kinetic_energy + 0.5d0 * mass(i) * velocity_squared
        END DO
    END FUNCTION T
! Computing the Acceleration
  SUBROUTINE compute_acc(Natoms, coord, mass, distance, acceleration, sigma, epsilon)
          IMPLICIT NONE
         INTEGER, INTENT(IN) :: Natoms
         DOUBLE PRECISION, INTENT(IN) :: coord(Natoms,3)
         DOUBLE PRECISION, INTENT(IN) :: mass(Natoms)
         DOUBLE PRECISION, INTENT(IN) :: distance(Natoms,Natoms)
         DOUBLE PRECISION, INTENT(IN) :: epsilon, sigma
         DOUBLE PRECISION, INTENT(OUT) :: acceleration(Natoms, 3)
        DOUBLE PRECISION :: rij, dx, dy, dz, inverse_m, force
         INTEGER :: i, j
 
         !Initialize acceleration to Zero 
         acceleration = 0.0
 
         ! Loop over all atoms 
         DO i = 1, Natoms
         inverse_m = 1.0 / mass(i)
         DO j = 1, Natoms
                 IF (j .gt. i) THEN
                         rij = distance(i,j)
 
                         ! Force from Lennard jones potential
                         force = 24.0 * epsilon * (2.0 * (sigma / rij)**12 - (sigma / rij)**6) / rij
 
                        ! Compute differences in Coordinates 
                         dx = coord(i,1) - coord(j,1)
                         dy = coord(i,2) - coord(j,2)
                         dZ = coord(i,3) - coord(j,3)
 
 
                         ! Update accelerations
                         acceleration(i,1) = acceleration(i,1) + inverse_m * force * (dx / rij)
                         acceleration(i,2) = acceleration(i,2) + inverse_m * force * (dx / rij)
                         acceleration(i,3) = acceleration(i,3) + inverse_m * force * (dx / rij)
 
                         ! Update accelerations
                         acceleration(j,1) = acceleration(j,1) + inverse_m * force * (dx / rij)
                         acceleration(j,2) = acceleration(j,2) + inverse_m * force * (dx / rij)
                         acceleration(j,3) = acceleration(j,3) + inverse_m * force * (dx / rij)
                 END IF
         END DO
END DO  
END SUBROUTINE compute_acc

SUBROUTINE update_r(natoms, r, v, a, ts)
    IMPLICIT NONE

    INTEGER, INTENT(IN) :: natoms
    REAL*8, INTENT(INOUT) :: r(natoms, 3)
    REAL*8, INTENT(IN) ::  v(natoms, 3), a(natoms, 3)
    REAL, INTENT(IN) :: ts

    INTEGER :: i, j

    DO i=1,natoms
    DO j=1,3
    r(i,j) = r(i,j) + v(i,j)*ts + a(i,j) * (ts**2) / 2
    END DO
    END DO
END SUBROUTINE

SUBROUTINE update_v(natoms, r, v, a, ts)
    IMPLICIT NONE

    INTEGER, INTENT(IN) :: natoms
    REAL*8, INTENT(INOUT) :: v(natoms, 3)
    REAL*8, INTENT(IN) ::  r(natoms, 3), a(natoms, 3)
    REAL, INTENT(IN) :: ts

    INTEGER :: i, j

    DO i=1,natoms
    DO j=1,3
    v(i,j) = v(i,j) + 0.5 * a(i,j) * ts
    END DO
    END DO
END SUBROUTINE



END MODULE MD_functions
