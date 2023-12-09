!> @file helloworld.c
!> @brief This example is to write the MPI hello world.
!> @details You will find two folders in this example, and will in all examples
!> provided in this session:
!> - src: contains the source code.
!> - bin: contains the binary produced.
!> The makefile provided already sets everything up for you:
!> - To compile: `make`.
!> - To execute: `sbatch submission.slurm`.
!> If you have any questions, do not hesitate.
!> @author Ludovic Capelli (l.capelli@epcc.ed.ac.uk)

!> @brief This program simply prints a hello world message, displaying the MPI
!> process identifier as well as the total number of MPI processes
!> @details Both variables must be properly initialised.
PROGRAM main
	USE mpi_f08

	IMPLICIT NONE
	
	! Initialise the program
	!> The rank of the MPI process running this instance.
	INTEGER :: my_rank = 0
	!> The number of MPI processes in MPI_COMM_WORLD.
	INTEGER :: comm_size = 0
	WRITE(*, '(A,I0,A,I0,A)') 'Hello world, I am MPI process ', my_rank, '. We are ', comm_size, ' MPI processes.'
	! Finalize the program
END PROGRAM main
