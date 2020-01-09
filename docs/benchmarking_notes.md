# Goal

Measure and benchmark the new precise unmarshalling to enable some targeted performance improvements.

## Notes

See `script/profile` for running benchmarks and producing profiles.

<details>
<summary>First baseline</summary>

NOTE: These are hard to compare as some were taken with profiling enabled and others not. cabal builds binaries for libraries with and without profiling (both .o and .p_o files), but executable are only built with the current flags.

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

</details>

## Alternative baseline(s)

Including baselines with and without profiling enabled.

Compiled with optimization and profiling

```
benchmarks: True
optimization: True
tests: True
profiling: True
profiling-detail: all-functions

benchmarking parsing/ruby ... took 10.67 s, total 56 iterations
benchmarked parsing/ruby
time                 199.8 ms   (194.6 ms .. 205.1 ms)
                     0.998 R²   (0.995 R² .. 1.000 R²)
mean                 191.0 ms   (186.1 ms .. 194.4 ms)
std dev              7.579 ms   (4.293 ms .. 11.86 ms)
```

---

Compiled with optimization but no profiling

```
benchmarks: True
optimization: True
tests: True
profiling: False

Up to date
benchmarked parsing/ruby
time                 71.99 ms   (69.16 ms .. 75.91 ms)
                     0.995 R²   (0.989 R² .. 0.999 R²)
mean                 70.11 ms   (68.69 ms .. 71.66 ms)
std dev              2.806 ms   (1.965 ms .. 4.370 ms)
```

## With b tree matchers

Compiled with optimization and profiling

```
benchmarks: True
optimization: True
tests: True
profiling: True
profiling-detail: all-functions

benchmarking parsing/ruby ... took 15.81 s, total 56 iterations
benchmarked parsing/ruby
time                 295.9 ms   (282.4 ms .. 332.7 ms)
                     0.984 R²   (0.957 R² .. 0.999 R²)
mean                 280.8 ms   (267.5 ms .. 299.5 ms)
std dev              23.29 ms   (14.77 ms .. 31.79 ms)
variance introduced by outliers: 28% (moderately inflated)
```

---

Compiled with optimization but no profiling

```
benchmarks: True
optimization: True
tests: True
profiling: False

benchmarked parsing/ruby
time                 51.67 ms   (51.01 ms .. 52.32 ms)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 50.58 ms   (49.77 ms .. 51.15 ms)
std dev              1.329 ms   (927.3 μs .. 1.685 ms)
```

NOTES: slightly faster here.

## With IntMap matchers

Compiled with optimization and profiling

```
benchmarks: True
optimization: True
tests: True
profiling: True
profiling-detail: all-functions

benchmarking parsing/ruby ... took 13.38 s, total 56 iterations
benchmarked parsing/ruby
time                 228.4 ms   (200.3 ms .. 243.7 ms)
                     0.984 R²   (0.958 R² .. 0.999 R²)
mean                 245.5 ms   (233.0 ms .. 272.6 ms)
std dev              27.34 ms   (16.01 ms .. 40.56 ms)
variance introduced by outliers: 38% (moderately inflated)
```

---

Compiled with optimization but no profiling

```
benchmarks: True
optimization: True
tests: True
profiling: False

benchmarked parsing/ruby
time                 64.81 ms   (58.99 ms .. 70.49 ms)
                     0.981 R²   (0.943 R² .. 0.998 R²)
mean                 65.60 ms   (63.48 ms .. 69.94 ms)
std dev              5.054 ms   (2.421 ms .. 7.985 ms)
variance introduced by outliers: 24% (moderately inflated)
```

## Specializing runMatch's monad transformer stack and some inlining

```
benchmarks: True
optimization: True
tests: True
profiling: False

benchmarked parsing/ruby
time                 13.47 ms   (13.21 ms .. 13.87 ms)
                     0.992 R²   (0.975 R² .. 0.999 R²)
mean                 13.59 ms   (13.45 ms .. 13.90 ms)
std dev              556.5 μs   (236.0 μs .. 1.048 ms)
variance introduced by outliers: 15% (moderately inflated)
```
