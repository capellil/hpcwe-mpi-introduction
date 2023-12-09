!> @file collectives.c
!> @brief This example is to write a program where MPI processes have to
!> perform a series of collective operations.
!> @details You will find two folders in this example, and will in all examples
!> provided in this session:
!> - src: contains the source code.
!> - bin: contains the binary produced.
!> The makefile provided already sets everything up for you:
!> - To compile: `make`.
!> - To execute: `sbatch submission.slurm`.
!> If you have any questions, do not hesitate.
!> @author Ludovic Capelli (l.capelli@epcc.ed.ac.uk)

!> @brief In this program, MPI processes print their rank, in turn.
!> @details This program consists of an MPI process picking the rank of the MPI
!> process that will act as the root throughout the application. This rank is
!> then broadcast to all MPI processes before a scatter and a reduction occur.
PROGRAM main
	USE mpi_f08

	IMPLICIT NONE

	INTEGER :: my_rank
	INTEGER :: comm_size
	INTEGER :: root_rank
	INTEGER, PARAMETER :: count = 2
	INTEGER, DIMENSION(:), ALLOCATABLE :: values
	INTEGER :: i
	INTEGER :: en
	INTEGER, DIMENSION(0:count-1) :: my_elements
	REAL :: rn
	INTEGER :: my_total = 0
	INTEGER :: total

	! Initialise the program
	CALL MPI_Init()

	! Get basic information.
	CALL MPI_Comm_rank(MPI_COMM_WORLD, my_rank)
	CALL MPI_Comm_size(MPI_COMM_WORLD, comm_size)

	! Part 1: if I am MPI process 0, I pick the rank of the MPI process that will act as the root, then broadcast it.
	IF (my_rank .EQ. 0) THEN
		root_rank = MOD(INT(rn), comm_size)
	END IF
	! TODO: Broadcast the root rank picked to all MPI processes.

	ALLOCATE(values(0:comm_size * count))
	! Part 2: the root MPI process declares an array and scatters it across all MPI processes, 2 consecutive integers per MPI process.
	IF (my_rank .EQ. root_rank) THEN
		DO i = 0, comm_size - 1
			CALL RANDOM_NUMBER(rn)
			values(i) = MOD(INT(rn), 20)
		END DO
	END IF
	! TODO: Scatter "values", sending "count" elements to each MPI process, and receive them in "my_elements".
	DO i = 0, count -1
		my_total = my_total + fibonacci(my_elements(i))
	END DO

	! Part 3: every MPI process sums
	! TODO: reduce the "my_total" of each MP process into the "total" variable on the root.
	IF (my_rank .EQ. root_rank) THEN
		WRITE(*, '(A,I0)') 'The sum of all fibonacci''s is ', total
	END IF

	! Finalize the program
	CALL MPI_Finalize();

	CONTAINS

	RECURSIVE INTEGER FUNCTION Fibonacci(n) RESULT(res)
		INTEGER, INTENT(IN) :: n

		IF (n .LE. 0) THEN
		res = 0
		ELSEIF (n .EQ. 1) THEN
		res = 1
		ELSE
		res = Fibonacci(n - 1) + Fibonacci(n - 2)
		END IF
	END FUNCTION Fibonacci
END PROGRAM main