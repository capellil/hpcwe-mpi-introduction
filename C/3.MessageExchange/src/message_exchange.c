/**
 * @file message_exchange.c
 * @brief This example is to write a program where a message is exchanged.
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
 * @brief This program requires 2 MPI processes to exchange messages.
 * @details In this program, an MPI process first sends one integer, which is
 * received then printed. The second message exchange is an array of four
 * elements.
 **/
int main(int argc, char* argv[])
{
	// Initialise the program

	// Get your rank
	int my_rank;
	if(my_rank == 0)
	{
		// Declare an int with value 123 and send it to MPI process 1
	}
	else
	{
		// Receive this integer from MPI process 0, print its value and do not use an MPI_Status object.
	}

	if(my_rank == 1)
	{
		// Declare and send an array of 4 elements (0, 1, 2, 3), using tag 789.
	}
	else
	{
		// Receive the array of 4 elements, without restricting the tag to use. Then, print all array elements and the tag value.
	}

	// Finalize the program

	return EXIT_SUCCESS;
}
