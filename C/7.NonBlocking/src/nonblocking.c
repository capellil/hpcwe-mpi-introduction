/**
 * @file nonblocking.c
 * @brief This example illustrates the use of non-blocking operations.
 * @details You will find two folders in this example, and will in all examples
 * provided in this session:
 * - src: contains the source code.
 * - bin: contains the binary produced.
 *
 * The makefile provided already sets everything up for you:
 * - To compile: `make`.
 * - To execute: `sbatch submission.slurm`.
 *
 * If you have any questions, do not hesitate.
 * @author Ludovic Capelli (l.capelli@epcc.ed.ac.uk)
 **/

#include <stdio.h>
#include <stdlib.h>
#include <mpi.h>

/**
 * @brief In this program, MPI processes print their rank, in turn.
 * @details In this program, two MPI processes exchanges messages. However, to
 * prevent deadlocks, they must do so in a specific order where the first MPI
 * process sends before receiving, and the second MPI process does the opposite.
 * The objective is to use non-blocking operations to remove the need for this
 * specific ordering of operations.
 **/
int main(int argc, char* argv[])
{
	// Initialise the program
	MPI_Init(&argc, &argv);

	int my_rank;
	MPI_Comm_rank(MPI_COMM_WORLD, &my_rank);
	int comm_size;
	MPI_Comm_size(MPI_COMM_WORLD, &comm_size);

	if(comm_size != 2)
	{
		printf("This application is meant to be run with 2 MPI processes, not %d.\n", comm_size);
		MPI_Abort(MPI_COMM_WORLD, EXIT_FAILURE);
	}

	// Restriction: you cannot replace the MPI_Ssend with MPI_Send or MPI_Bsend.
	int received = 0;
	if(my_rank == 0)
	{
		int to_send = 123;
		// Send first, then receive.
		MPI_Ssend(&to_send, 1, MPI_INT, 1, 0, MPI_COMM_WORLD);
		MPI_Recv(&received, 1, MPI_INT, 1, 0, MPI_COMM_WORLD, MPI_STATUS_IGNORE);
	}
	else
	{
		int to_send = 456;
		// Receive first, then send.
		MPI_Recv(&received, 1, MPI_INT, 0, 0, MPI_COMM_WORLD, MPI_STATUS_IGNORE);
		MPI_Ssend(&to_send, 1, MPI_INT, 0, 0, MPI_COMM_WORLD);
	}

	printf("[MPI process %d] The value I received is %d.\n", my_rank, received);

	// Finalize the program
	MPI_Finalize();

	return EXIT_SUCCESS;
}
