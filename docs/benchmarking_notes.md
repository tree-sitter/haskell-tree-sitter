# Goal

Measure and benchmark the new precise unmarshalling to enable some targeted performance improvements.

## Notes

See `script/profile` for running benchmarks and producing profiles.

## Baseline

Using *.rb glob in:
`../semantic/tmp/ruby-examples/ruby_spec/command_line`

Benchmark benchmarks: RUNNING...
benchmarked parsing/ruby
time                 74.59 ms   (69.31 ms .. 85.18 ms)
                     0.975 R²   (0.935 R² .. 0.998 R²)
mean                 76.93 ms   (74.29 ms .. 79.79 ms)
std dev              5.228 ms   (3.679 ms .. 7.447 ms)
variance introduced by outliers: 17% (moderately inflated)

Benchmark benchmarks: FINISH

## Optimization: Using a table of matchers

Using an IntMap of matchers instead of chain `if then` for symbol matching.

benchmarked parsing/ruby
time                 25.10 ms   (20.56 ms .. 33.29 ms)
                     0.654 R²   (0.489 R² .. 0.972 R²)
mean                 24.32 ms   (21.84 ms .. 29.52 ms)
std dev              7.700 ms   (4.905 ms .. 11.64 ms)
variance introduced by outliers: 89% (severely inflated)

*This was an erroneous result as we didn't properly contruct the IntMap for choice.*

## Optimization: Using a table of matchers (actually implement choice)

benchmarking parsing/ruby ... took 9.376 s, total 56 iterations
benchmarked parsing/ruby
time                 153.7 ms   (128.2 ms .. 167.1 ms)
                     0.959 R²   (0.861 R² .. 0.999 R²)
mean                 176.0 ms   (163.4 ms .. 194.8 ms)
std dev              27.36 ms   (12.27 ms .. 38.39 ms)
variance introduced by outliers: 48% (moderately inflated)

:( Made it slower...

## Optimization: Rob's (reicarnated as Alonzo) latest datastructure (church encoded binary tree)

benchmarking parsing/ruby ... took 8.321 s, total 56 iterations
benchmarked parsing/ruby
time                 154.1 ms   (144.4 ms .. 162.1 ms)
                     0.995 R²   (0.990 R² .. 0.999 R²)
mean                 149.8 ms   (144.3 ms .. 154.3 ms)
std dev              8.540 ms   (5.969 ms .. 12.65 ms)
variance introduced by outliers: 18% (moderately inflated)

*About the same :(*

## From semantic

With the
