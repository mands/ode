// ODE Runtime Library Header file
#ifndef SSA_SOLVER_H
#define SSA_SOLVER_H
#include <stdint.h>

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
void runSimulation(const char* filename, const double stopTime, const double outPeriod,
                   const uint64_t maxPopulation, Reaction reactions[], const uint64_t numReactions,
                   const Species* restrict species[], const uint64_t numSpecies);
inline double chooseTimestep(const double sumProp);
inline uint64_t chooseReaction(const double sumProp, const Reaction reactions[], const uint64_t numReactions);
inline void triggerReaction(Reaction r);
inline double sumPropensities(const Reaction reactions[], const uint64_t numReactions);
inline double calcPropensity(const Reaction r);

void writeState(const double time, const Species* restrict species[], const uint64_t numSpecies);
void checkPopulations(const Species* restrict species[], const uint64_t numSpecies, const uint64_t maxPopulation);

#endif // SSA_SOLVER_H

