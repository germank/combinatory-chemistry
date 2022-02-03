# Combinatory Chemistry

This is the code for simulating the system described in 
"Emergence of Self-Reproducing Metabolisms as Recursive Algorithms in an Artificial Chemistry" by
Germán Kruszewski and Tomas Mikolov. 
https://arxiv.org/abs/2103.08245


## Requirements

Running this code requires [clojure](https://clojure.org/guides/getting_started)
and the [leiningen](https://leiningen.org/) project manager.

## Usage

`lein run`  will start a simulation reporting some statistics on the standard
output. 

A better way is running it to log information to the disk, as follows:

`lein run --log LOG_DIRECTORY --scrolling-log`. 

This will log information to `metrics.edn` and`reactions.log` files in the given
log directory. The second argument is to avoid showing information on the 
standard output, which makes the simulation slower.

Some important arguments are `--size` which controls the total number of 
combinators in the system, `--time` to control the simulation length and
`--threads` for multi-processing support. 

Other options are listed with `lein run -H`.

## Examples

The following command runs the simulation for 1000 time units, initializing
the system with 10k uniformly distributed S, K and I combinators:

`lein run --scrolling-log --size=10000 --time=1000 --log "my-experiment"`

Reaction rate constants are defined with `--condensation-rate`, `--reduction-rate`
(for K and I combinators), and `--S-reduction-rate` (for the S combinator).
The default values correspond to those reported in the paper.

The volume of the container is controlled with `--volume` and set to be equal
to `--size` by default.

## Log files

`metrics.edn` log some statistics, like number of expressions, diversity, etc.
at every unit interval of time.

`reactions.log` contain every single reaction that was registered in the system.
This can be leveraged, for instance, to plot reactant consumption rates.

## Plots

### Dependencies
Plotting is based on the [oz](https://github.com/metasoarous/oz) package. See
the documentation for troubleshooting and installation requirements.

### Usage

The recommended way to plot is using the REPL (either from your editor or
directly in the command line). All plots can be produced with the `plot-selected!`
helper function. 


For instance, to plot the consumption of a set of selected reactants
weighted by the information metric, you can use:

```
=> (use 'cc.plot)
=> (plot-selected! "<log-directory-path (without a filename)>" 
                  "<output-path>"
                  [:reactants-consumption]
                  {
                  :only-keys '[S K I (S S) (K I) (K K) (S K) (S I) (S I I) (S (S I) I)]}]
                  :smoothing 20
                  :apply-weighting true ;set to to false to use raw counts
                  :dest-filename "selected-reactants-by-consumption.svg"})
```

If you set the output path to be `nil`, then the plot is displayed in a web server
available at `localhost:10666`. 

See `extra/plots.clj` for code snippets to reproduce the figures in the paper.
