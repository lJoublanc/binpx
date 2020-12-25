# Example

    ghc Main.hs && ./Main

  or load the module in GHCi and call the `price` function.

# Dependencies

`recursion-schemes` and it's deps
  
# About
This is an implementation of binomial option pricing, following the example from CQF module 1 (2019)

Some notes/differences with respect to the Excel implementation:

  * The delta caluclation is actually **not** required to derive the option price (though it may be useful on it's own), so we omit it.
  * `u` and `v` are not free parameters; they are derived from σ and δt. 
  * We calculate δt by providing number of divisions of T instead of the direct value.
  * Rounding error may cause δt to not divide evenly into T (Set `n` to a power of 2).
  * In the current implementation we calcualte `O(n²)` nodes for clarity. I think this could be fixed using a `futu`morphism.
