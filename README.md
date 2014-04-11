# _Ode_ Modelling DSL

_Ode_ is a biological modelling DSL for describing spatially-homogeneous mathematical biological models comprised from ODEs, SDEs, and SSA-reactions.
It consists of both a modular modelling language and a high-performance simulation backend using the LLVM compiler framework.

On this page:

* [Background](#markdown-header-background)
* [_Ode_ DSL](#markdown-header-ode-dsl)
* [Model Simulation](#markdown-header-model-simulation)
* [Sample Models](#markdown-header-sample-models)
* [Building](#markdown-header-building)
* [Links](#markdown-header-links)
* [License](#markdown-header-license)

## Background

_Ode_ was created as part of research to investigate the application of software engineering practices to the mathematical biological modelling domain, with an initial focus on cardiac electrophysiological models. This work was undertaken within the Computational Biology research group within the Department of Computer Science at the University of Oxford ([homepage](http://www.cs.ox.ac.uk/research/compbio)).
It involved research into type-checking, modularity, model optimisation and efficient hybrid-simulation.
The accompanying D.Phil thesis resulting from this work may be also be downloaded from this website.

Several papers were published supporting this work, 

* M. Gill, S. McKeever, and D.J. Gavaghan. "Modular Mathematical Modelling of Biological Systems". In: _Symposium on Theory of Modeling and Simulation (TMS’12)_. 2012.
* M. Gill, S. McKeever, and D.J. Gavaghan. “Modules for Reusable and Collaborative Modelling of Biological Mathematical Systems”. In: _21ST IEEE International WETICE Conference (WETICE-2012)_. 2012.
* S. McKeever, M. Gill, A.J. Connor, and D. Johnson. “Abstraction in physiological modelling languages”. In: _Symposium on Theory of Modeling & Simulation (TMS’13)_. 2013.
* M. Gill, S. McKeever, and D.J. Gavaghan. “Model Composition for Biological Mathematical Systems”. In: _International Conference on Model-Driven Engineering and Software Development (Modelsward 2014)_. 2014.

## _Ode_ DSL

_Ode_ is a simple numerical language with a syntax inspired by Python and MATLAB in order to remain familar to modellers. The base language was extended with support for type- and units-of-measure inferences, ML-style modular constructs (including parametrised modules), and first-class support for Ordinary Differential Equations (ODEs), Stochastic Differential Equations (SDEs) and SSA-style discrete chemical reactions.

Further details on the _Ode_ language can be found in the accompanying thesis.

## Model Simulation

_Ode_ provides a REPL interface through which model repositories may be activated and model modules loaded into the active environment. Active _Ode_ modules may be combined into a simulation-ready model and the simulation parameters also configured. The model, containing ODEs, SDEs, and/or SSA-reactions, may be simulated using an internal interpreter, or compiled to a native representation using the LLVM framework for more-efficient simulation.

Further details and simulation benchmarks can be found in the accompanying thesis.

## Sample Models

Several versions of the Hodgkin-Huxley giant-squid axon neuronal model [1] are included to demonstrate the language functionality. Further, cardiac electrophysiological, models are to be added.

[1] - A.L. Hodgkin, and A.L. Huxley. "A quantitative description of membrane current and its application to conduction and excitation in nerve". In: _The Journal of physiology 117 (4): 500–544_. (1952).

## Building

_Ode_ was developed using Haskell and requires a recent version of the GHC compiler (7.2+ should be fine). The system was developed under Linux however the language core should function on most platforms supported by GHC.
Simulation via LLVM requires a recent release of both LLVM and Clang (3.3+ should be fine). The main source is located in the `ode3` directory, with the `res` directory including utilities and resources.

~~~
cabal configure
cabal build
cabal install
~~~

Once built, `ode` can be executed from the command-line without options.

## Links

This work was performed within the Computational Biology research group within the Department of Computer Science at the University of Oxford. Several related links,

 * [Accompanying Thesis](?)
 * [Computational Biology Research Group](http://www.cs.ox.ac.uk/research/compbio)
 * [Chaste Modelling Framework](http://www.cs.ox.ac.uk/chaste)
 * [CellML Modelling Language](http://www.cellml.org)


----

## License

All source code created as part of the _Ode_ DSL is released under the BSD-3 license. The license file is located [here](https://bitbucket.org/mands/ode/src/master/LICENSE).

