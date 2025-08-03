#include <iostream>
#include "Vmotherboard.h"
#include "verilated.h"

// to build, run the code just below
// verilator --cc --build -I./control_section/ motherboard.v
// That command outputs an executable, Vmotherboard, in the obj_dir/ directory

using namespace std;

int main(int argc, char **argv) {
    Verilated::commandArgs(argc, argv);

    Vmotherboard* top = new Vmotherboard;

    cout << "Hello with Verilator!" << endl;

    delete top;

    return 0;
}

