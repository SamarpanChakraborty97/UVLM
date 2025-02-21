#include <stdio.h>
#include <iostream>
#include "AuxVariable.h"
#include "GeneralModule.h"
#include "ElasticModule.h"
#include "AerodynamicModule.h"
#include <vector>
#include <chrono>

const int num_input_files = 1;
const int num_output_files = 15;
const int MAXZONES = 10000;

int main(int argc, char* argv[]) {
    struct GeneralData* GD = new GeneralData;
    struct AeroBody* AB = new AeroBody;
    struct Wake* AW = new Wake;
    struct Spring* S = new Spring;
    struct AuxVariable* AV = new AuxVariable;

    std::string suf;
    std::vector <int> IFS(num_input_files);
    std::vector <int> OFS(num_output_files);

    int IT, b, Zones;                   // initialization of some variables
    int FileCount, kont;                // count of files and iteration count

    // Measure execution time
    auto start_time = std::chrono::high_resolution_clock::now(); // Start time

    /// PRE_PROCESSING ///
    switch (argc)
    {
    case 1:
        printf("Only the input file has been provided!");
        // open the input files
        void readAndPopulateStructures()

    case 2:
        printf("Both the input file and the output directory names have been provided!");
    default:
        printf("Too many input arguments!");
        return 0;
    };
    

    // Measure execution time
    auto end_time = std::chrono::high_resolution_clock::now(); // End time
    auto duration = std::chrono::duration_cast<std::chrono::milliseconds>(end_time - start_time).count();

    std::cout << "Execution time: " << duration << " milliseconds" << std::endl;

    return 0;
};