!> @file synchronous_or_asynchronous.c
!> @brief This example is to write a program where a message is sent in a
!> synchronous fashion, then asynchronous.
!> @details You will find two folders in this example, and will in all examples
!> provided in this session:
!> - src: contains the source code.
!> - bin: contains the binary produced.
!> The makefile provided already sets everything up for you:
!> - To compile: `make`.
!> - To execute: `sbatch submission.slurm`.
!> If you have any questions, do not hesitate.
!> @author Ludovic Capelli (l.capelli@epcc.ed.ac.uk)

!> @brief This program requires 2 MPI processes to exchange messages, trying
!> both synchronous and asynchronous methods.
PROGRAM main
	USE mpi_f08

	IMPLICIT NONE

	INTEGER :: my_rank = 0

	! Initialise the program

	! Get your rank
	IF (my_rank == 0) THEN
		! Declare an int with value 123 and send it to MPI process 1 synchronously.
	ELSE
		! Receive this integer from MPI process 0 and print its value.
	END IF

	IF (my_rank == 1) THEN
		! Declare and send an array of 4 elements (0, 1, 2, 3), and send it asynchronously.
		! Declare and send an array of 5 elements (4, 5, 6, 7, 8), and send it asynchronously.
	ELSE
		! Receive the array of 4 elements and print the array elements.
		! Receive the array of 5 elements and print the array elements.
	END IF

	! Finalize the program
END PROGRAM main