// a basic SSA solver, includes hardcoded models and utilises Ode Stdlib
#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>
#include <inttypes.h>
#include <math.h>

#include "SSASolver.h"
#include "OdeStdLib.h"

//static const uint64_t maxUint64 = 0xFFFFFFFFFFFFFFFF;

// take in a model and actually run the simulation using SSA
void runSimulation(const char* filename, const double stopTime, const double outPeriod,
                   const uint64_t maxPopulation, Reaction reactions[], const uint64_t numReactions,
                   const Species* restrict species[], const uint64_t numSpecies) {
    OdeStartSim(filename, numSpecies);
    // run ssa
    double time = 0, tau;
    uint64_t loopCount = 1;
    double nextOutput = loopCount * outPeriod;
    // initial output
    writeState(time, species, numSpecies);

    double sumProp = sumPropensities(reactions, numReactions);
    while (sumProp > 0 && time < stopTime) {
        if (maxPopulation) checkPopulations(species, numSpecies, maxPopulation);
        // get the stochastic time-step
        tau = chooseTimestep(sumProp);
        // calc the reaction
        Reaction r = reactions[chooseReaction(sumProp, reactions, numReactions)];
        // finally update the system state consdiering the reaction R occured at time tau
        triggerReaction(r);
        // inc time as needed
        time += tau;

        // output the current state
        if(time >= nextOutput) {
            ++loopCount;
            nextOutput = (double)loopCount * outPeriod;
            writeState(time, species, numSpecies);
        }

        // calculuate the sum of all the reaction propensities within the system for next iteration
        sumProp = sumPropensities(reactions, numReactions);
    }
    // write final state
    // writeState(time, species, numSpecies);
    OdeStopSim();
}

// randomly chooses a time for the reaction to occur
inline double chooseTimestep(const double sumProp) {
    double r1 = OdeRandUniform();
    return -(1/sumProp) * log(r1);
}

// randomly chooses a reaction from the set
inline uint64_t chooseReaction(const double sumProp, const Reaction reactions[], const uint64_t numReactions) {
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

// updates all species invovled when reaction R occurs
inline void triggerReaction(Reaction r) {
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

// return the sum of all reaction propensitites
inline double sumPropensities(const Reaction reactions[], const uint64_t numReactions) {
    double sumProp = 0;
    for (uint64_t idx = 0; idx < numReactions; ++idx) {
        sumProp += calcPropensity(reactions[idx]);
    }
    return sumProp;
}

// calculate the propensitiy of a reaction
inline double calcPropensity(const Reaction r) {
    switch (r.reactionType) {
    case UniMolecular:
        return ((*r.speciesA) * r.rate);
    case BiMolecular:
        return ((*r.speciesA) * (*r.speciesB) * r.rate);
    }
}

// write the current state of all species to disk
void writeState(const double time, const Species* restrict species[], const uint64_t numSpecies) {
    // need to convert the array from uint to double
    double outArr[numSpecies];
    uint64_t idx;
    for (idx = 0; idx < numSpecies; ++idx) {
        outArr[idx] = (double)*(species[idx]);
    }
    OdeWriteState(time, outArr);
}

void checkPopulations(const Species* restrict species[], const uint64_t numSpecies, const uint64_t maxPopulation) {
    for (uint64_t idx = 0; idx < numSpecies; ++idx) {
        if (*(species[idx]) > maxPopulation) {
            fprintf(stderr,
                    "Model has become unbounded, species population - %" PRIu64 ", max allowed - %" PRIu64 "\n",
                    *(species[idx]), maxPopulation);
            exit(EXIT_FAILURE);
        }
    }
}
