/**
 * @file collectives.c
 * @brief This example is to write a program where MPI processes have to
 * perform a series of collective operations.
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

int fibonacci(int n)
{
	if(n < 2)
	{
		return n;
	}
	return fibonacci(n-1) + fibonacci(n-2);
}

/**
 * @brief In this program, MPI processes print their rank, in turn.
 * @details This program consists of an MPI process picking the rank of the MPI
 * process that will act as the root throughout the application. This rank is
 * then broadcast to all MPI processes before a scatter and a reduction occur.
 **/
int main(int argc, char* argv[])
{
	// Initialise the program
	MPI_Init(&argc, &argv);

	// Get basic information.
	int my_rank;
	MPI_Comm_rank(MPI_COMM_WORLD, &my_rank);
	int comm_size;
	MPI_Comm_size(MPI_COMM_WORLD, &comm_size);

	// Part 1: if I am MPI process 0, I pick the rank of the MPI process that will act as the root, then broadcast it.
	int root_rank;
	if(my_rank == 0)
	{
		root_rank = rand() % comm_size;
	}
	// TODO: Broadcast the root rank picked to all MPI processes.

	int count = 2;
	int values[comm_size * count];
	// Part 2: the root MPI process declares an array and scatters it across all MPI processes, 2 consecutive integers per MPI process.
	if(my_rank == root_rank)
	{
		for(int i = 0; i < comm_size * count; i++)
		{
			values[i] = rand() % 20;
		}
	}
	int my_elements[count];
	// TODO: Scatter "values", sending "count" elements to each MPI process, and receive them in "my_elements".
	int my_total = 0;
	for(int i = 0; i < count; i++)
	{
		my_total += fibonacci(my_elements[i]);
	}

	// Part 3: every MPI process sums
	int total;
	// TODO: reduce the "my_total" of each MP process into the "total" variable on the root.
	if(my_rank == root_rank)
	{
		printf("The sum of all fibonacci's is %d\n", total);
	}

	// Finalize the program
	MPI_Finalize();

	return EXIT_SUCCESS;
}
