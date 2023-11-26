/**
 * @file collectives.c
 * @brief This example is to write a program where MPI processes have to
 * coordinate.
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
 * @details This program can adapt to any number of MPI processes. MPI processes
 * all print their rank, in the order of their rank. In other words, the MPI 
 * process 0 must print its rank first, then MPI process 1 prints it rank, then
 * MPI process 2 etc...
 **/
int main(int argc, char* argv[])
{
	// Initialise the program
	MPI_Init(&argc, &argv);

	int root_rank = rand() % 10;
	// bcast

	// Declare array
	if(my_rank == root_rank)
	{
		// initialise array
	}
	// scatter

	// reduce

	// Finalize the program
	MPI_Finalize();

	return EXIT_SUCCESS;
}
