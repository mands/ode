/* ODE FILE READER
** A simple file reader that takes a binary file and the number of colums as input
** These are used to output a matrix of doubles that may be redirected to the screen or a file as needed
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int main(int argc, char** argv) {
	if (argc != 2) {
		fprintf(stderr, "Usage - FileReader filename\n");
		exit(EXIT_FAILURE);
	}

	const char* filename = argv[1];
	// should check if file exists here
	FILE *file = fopen(filename, "rb");
	if (file == NULL) {
		fprintf(stderr, "Error opening file %s\n", filename);
		exit(EXIT_FAILURE);
	}

	// read the file columns
	int columns = 1;
	if (feof(file) || (fread(&columns, sizeof(int), 1, file) != 1)) {
		fprintf(stderr, "Cannot read columns from file %s\n", filename);
		exit(EXIT_FAILURE);		
	}

	if (columns == 0) {
		fprintf(stderr, "Read invalid number of columns from file %s\n", filename);
		exit(EXIT_FAILURE);
	}

	// parse file in blocks equiv to a single row  
	printf("Opened file %s with %d columns\n", filename, columns);
	double row[columns];
	int i;
	while (!feof(file)) {
		if (fread(&row, sizeof(double), columns, file) != 0) {
			for(i = 0; i < (columns); i++) {
				//printf("%8.5g,\t", row[i]);
                printf("%.16g,\t", row[i]);
			}
			printf("\n");
		}
	}

	fclose(file);
	exit(EXIT_SUCCESS);
}
