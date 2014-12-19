// adapted for Visual C++ from: https://github.com/rdbisch/Santa2014

#include "stdafx.h"
#include "Naive.h"


double score(int num_elves, string toysPath);

using namespace std;

int main(int argc, char** argv) {
    init_startTime();

    if ( argc < 3 ) {
usage_message:
        cerr << "Usage:\n\ta.out (--score) [n] [toys]\n\n"
            << "\t--score\tIf present will score the submission on stdin.  If not will produce naive solution.\n"
            << "\t[n]\tinteger number of elves\n"
            << "\t[toys]\tPath to list of toys.\n";
        std::exit(1);
    }
    DWORD dw = GetTickCount();
    if ( strcmp(argv[1], "--score") == 0 ) {
        if (argc < 4) goto usage_message;
        int n = atoi(argv[2]);
        string toys = string(argv[3]);
        cout << std::setprecision(15) << score( n, toys ) << endl;
    } else {
        NaiveSolution e( atoi(argv[1]), string(argv[2]) );
    }
    dw = GetTickCount() - dw;
    fprintf(stderr,"run time %u.%03u s\n", dw / 1000, dw % 1000);
    return 0;
}

