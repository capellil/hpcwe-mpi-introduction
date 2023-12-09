!> @file nonblocking.c
!> @brief This example illustrates the use of non-blocking operations.
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
!> @details In this program, two MPI processes exchanges messages. However, to
!> prevent deadlocks, they must do so in a specific order where the first MPI
!> process sends before receiving, and the second MPI process does the opposite.
!> The objective is to use non-blocking operations to remove the need for this
!> specific ordering of operations.
PROGRAM main
	USE mpi_f08

	IMPLICIT NONE

	INTEGER :: my_rank
	INTEGER :: comm_size
	INTEGER :: received
	INTEGER :: to_send

	! Initialise the program
	CALL MPI_Init()

	CALL MPI_Comm_rank(MPI_COMM_WORLD, my_rank)
	CALL MPI_Comm_size(MPI_COMM_WORLD, comm_size)

	IF (comm_size .NE. 2) THEN
		WRITE(*, '(A,I0,A)') 'This application is meant to be run with 2 MPI processes, not ', comm_size, '.'
		CALL MPI_Abort(MPI_COMM_WORLD, -1)
	END IF

	! Restriction: you cannot replace the MPI_Ssend with MPI_Send or MPI_Bsend.
	received = 0;
	IF (my_rank == 0) THEN
		to_send = 123;
		! Send first, then receive.
		CALL MPI_Ssend(to_send, 1, MPI_INTEGER, 1, 0, MPI_COMM_WORLD)
		CALL MPI_Recv(received, 1, MPI_INTEGER, 1, 0, MPI_COMM_WORLD, MPI_STATUS_IGNORE)
	ELSE
		to_send = 456;
		! Receive first, then send.
		CALL MPI_Recv(received, 1, MPI_INTEGER, 0, 0, MPI_COMM_WORLD, MPI_STATUS_IGNORE)
		CALL MPI_Ssend(to_send, 1, MPI_INTEGER, 0, 0, MPI_COMM_WORLD)
	END IF

	WRITE(*, '(A,I0,A,I0,A)') '[MPI process ', my_rank, '] The value I received is ', received, '.'

	! Finalize the program
	CALL MPI_Finalize()
END PROGRAM main