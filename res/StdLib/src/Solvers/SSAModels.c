#include <stdlib.h>

#include "SSASolver.h"
#include "OdeStdLib.h"

void RadioactiveDecaySim(void);
void PredPreySim(void);


int main(void) {
    OdeInit();
    //RadioactiveDecaySim();
    PredPreySim();
    OdeShutdown();
}

// hard-coded basic SSA simulation
void RadioactiveDecaySim(void) {
    // species
    // const uint64_t numSpecies = 2;
    Species x = 1000;
    Species z = 0;

    const Species* species[2] = {&x, &z};

    // reactions, using inline style
    ReactionProduct r1p[1] = { { &z, 1 } };
    Reaction r1 = { UniMolecular, &x, NULL, 0.5, 1, r1p };
//    Reaction r1 = {.reactionType=UniMolecular, .speciesA=&x,
//                   .speciesB=NULL, .rate=0.5, .numProducts=1,
//                   .products={ {.prodSpecits=&z, .stoc=1 } } };

    Reaction reactions[1] = {r1};

    // start the sim
    runSimulation("radiodecay.bin", 100, 0.1, 0, reactions, 1, species, 2);
}

void PredPreySim(void) {
    Species x = 1;
    Species y1 = 1000;
    Species y2 = 1000;
    Species z = 0;

    const Species* species[4] = {&x, &y1, &y2, &z};

    // reactions
    // r1 = x + y1 -> x + 2y1 (rate : 10)
    // prey w/ static birth rate (thus can grow unbounded if all predators die)
    ReactionProduct r1p[2] = { { &y1, 2 } , { &x, 1 } };
    Reaction r1 = { BiMolecular, &x, &y1, 10, 2, r1p };

    // r2 = y1 + y2 -> 2y2 (rate : 0.01)
    // predator - created in responce to prey
    ReactionProduct r2p[1] = { { &y2, 2 } };
    Reaction r2 = { BiMolecular, &y1, &y2, 0.01, 1, r2p };

    // r3 = y2 -> z (rate : 10)
    // predator death
    ReactionProduct r3p[1] = { { &z, 1 } };
    Reaction r3 = { UniMolecular, &y2, NULL, 10, 1, r3p };

    Reaction reactions[3] = {r1, r2, r3};

    // start the sim
    runSimulation("predprey.bin", 100, 0.001, 0, reactions, 3, species, 4);

}
