/**
 * @file helloworld.c
 * @brief This example is to write the MPI hello world.
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
 * @brief This program simply prints a hello world message, displaying the MPI
 * process identifier as well as the total number of MPI processes
 * @details Both variables must be properly initialised.
 **/
int main(int argc, char* argv[])
{
	// Initialise the program
	/// The rank of the MPI process running this instance.
	int my_rank = 0;
	/// The number of MPI processes in MPI_COMM_WORLD.
	int comm_size = 0;
	printf("Hello world, I am MPI process %d. We are %d MPI processes.\n", my_rank, comm_size);
	// Finalize the program

	return EXIT_SUCCESS;
}
