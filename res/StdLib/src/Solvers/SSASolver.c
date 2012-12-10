// a basic SSA solver, includes hardcoded models and utilises Ode Stdlib
#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stdint.h>
#include <string.h>
#include <math.h>

#include "OdeStdLib.h"

// Data structures
// species - only holds the population
typedef uint64_t Species;

// reactions
typedef enum ReactionTypesT {
    UniMolecular = 0,
    BiMolecular = 1
} ReactionTypes;

typedef struct ReactionProductT {
    Species* prodSpecies;
    uint64_t stoc;
} ReactionProduct;

typedef struct ReactionT {
    ReactionTypes reactionType;
    Species* speciesA;
    Species* speciesB;
    double rate;
    uint64_t numProducts;
    ReactionProduct* products;
} Reaction;


// function declarations
double calcPropensity(Reaction r);
double sumPropensities(Reaction reactions[], uint64_t numReactions);
double chooseTimestep(double sumProp);
uint64_t chooseReaction(double sumProp, Reaction reactions[], uint64_t numReactions);
void triggerReaction(Reaction r);
void writeState(double time, Species* species[], uint64_t numSpecies);
void runSimulation(Reaction reactions[], uint64_t numReactions, Species* species[],
                   uint64_t numSpecies, double stopTime);
void RadioactiveDecaySim(void);


// hard-coded basic SSA simulation
void RadioactiveDecaySim(void) {
    // species
    // const uint64_t numSpecies = 2;
    Species x = 1000;
    Species z = 0;

    Species* species[2] = {&x, &z};

    // reactions, using inline style
    ReactionProduct r1p[1] = { { &z, 1 } };
    Reaction r1 = { UniMolecular, &x, NULL, 0.5, 1, r1p };
//    Reaction r1 = {.reactionType=UniMolecular, .speciesA=&x,
//                   .speciesB=NULL, .rate=0.5, .numProducts=1,
//                   .products={ {.prodSpecits=&z, .stoc=1 } } };

    Reaction reactions[1] = {r1};

    // start the sim
    runSimulation(reactions, 1, species, 2, 100);

}

int main(void) {
    OdeInit();
    RadioactiveDecaySim();
    OdeShutdown();
}


// take in a model and actually run the simulation using SSA
void runSimulation(Reaction reactions[], uint64_t numReactions, Species* species[],
                   uint64_t numSpecies, double stopTime) {
    OdeStartSim("output.bin", numSpecies);
    // run ssa
    double time = 0, tau;
    double sumProp = sumPropensities(reactions, numReactions);
    while (time < stopTime && sumProp > 0) {
        // calculuate the sum of all the reaction propensities within the system
        sumProp = sumPropensities(reactions, numReactions);
        if (sumProp > 0) {
            // output the current state
            writeState(time, species, numSpecies);
            // get the stochastic time-step
            tau = chooseTimestep(sumProp);
            // calc the reaction
            Reaction r = reactions[chooseReaction(sumProp, reactions, numReactions)];
            // finally update the system state consdiering the reaction R occured at time tau
            triggerReaction(r);
            // inc time as needed
            time += tau;
        }
    }
    OdeStopSim();
}

void writeState(double time, Species *species[], uint64_t numSpecies) {
    // need to convert the array from uint to double
    double outArr[numSpecies];
    uint64_t idx;
    for (idx = 0; idx < numSpecies; ++idx) {
        outArr[idx] = (double)*(species[idx]);
    }
    OdeWriteState(time, outArr);
}

void triggerReaction(Reaction r) {
    switch (r.reactionType) {
    case UniMolecular:
        --(*r.speciesA);
        break;
    case BiMolecular:
        --(*r.speciesA);
        --(*r.speciesB);
        break;
    }

    ReactionProduct rp;
    for (uint64_t idx = 0; idx < r.numProducts; ++idx) {
        rp = r.products[idx];
        (*rp.prodSpecies) += rp.stoc;
    }
}

uint64_t chooseReaction(double sumProp, Reaction reactions[], uint64_t numReactions) {
    double r2 = OdeRandUniform();
    double endProp = r2 * sumProp;
    double curProp = 0;
    uint64_t idx = 0;
    while (curProp <= endProp) {
        curProp += calcPropensity(reactions[idx]);
        ++idx;
    }
    return (idx-1);
}

double chooseTimestep(double sumProp) {
    double r1 = OdeRandUniform();
    return -(1/sumProp) * log(r1);
}

double sumPropensities(Reaction reactions[], uint64_t numReactions) {
    double sumProp = 0;
    for (uint64_t idx = 0; idx < numReactions; ++idx) {
        sumProp += calcPropensity(reactions[idx]);
    }
    return sumProp;
}

double calcPropensity(Reaction r) {
    switch (r.reactionType) {
    case UniMolecular:
        return ((*r.speciesA) * r.rate);
    case BiMolecular:
        return ((*r.speciesA) * (*r.speciesB) * r.rate);
    }
}
