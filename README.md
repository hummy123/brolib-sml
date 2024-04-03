# Brolib-sml

## Introduction

Standard ML port of [this](https://github.com/hummy123/brolib) rope implementation.

This particular rope uses the balancing scheme described in the [Purely Functional 1-2 Brother Trees paper authored by Ralph Hinze](https://www.cs.ox.ac.uk/ralf.hinze/publications/Brother12.pdf). It tries to keep the number of nodes to a minimum by joining the strings in adjacent leaf nodes, if joining would not be too expensive.

## Usage

The two files are `rope.sml` and `tiny_rope.sml`. 

`rope.sml` contains a rope that tracks line metadata (which has a small performance and memory penalty). This is useful if you have line-based operations in mind.

`tiny_rope.sml` doesn't track line metadata, and is useful when line-queries aren't needed.

Except for the line-based operations `appendLine` and `foldLines`, all functions are the same between the two (aside from `verifyLines` which is just for testing purposes).

Examples of usage can be found in [`examples.sml`](https://github.com/hummy123/brolib-sml/blob/main/examples.sml).

## Performance

These two ropes are both quite fast. 

I compared the OCaml port with the other text data structures in OCaml, and it beat those handily when processing the datasets from [here](https://github.com/josephg/editing-traces) which just test insertion and deletion. It was also faster at performing substrings than the others.

I don't know other Standard ML libraries to compare it to, but with MLton, this rope implementation beats [the fastest ropes in Rust](https://github.com/josephg/jumprope-rs#benchmarks) at insertion and deletion quite easily, never going 1 ms in the slowest dataset. 

I don't know how to explain this surprising result, but most of the credit must go to the MLton compiler. This result might also be explained by some entirely untested theories that may or may not be true:

- MLton may have optimised the data set (which is pure Standard ML)
- These benchmarks have an unfair advantage because the datasets are cache-friendly vectors/arrays.
- These ropes are likely slower on queries (those Rust ropes use B-Trees which are more cache-friendly).
- The other ropes may track more metadata (like UTF-8/16/32 indices) which would add take a little more time.

Here are some numbers in nanoseconds, running on a single core with a Raspberry Pi 5 that has 8 GB of RAM:

| Dataset         | rope.sml time | tiny_rope.sml time |
|-----------------|---------------|--------------------|
| automerge-paper | 10,018 ns     | 9,726 ns           |
| rustcode        | 79,896 ns     | 74,479 ns          |
| sveltecomponent | 280,654 ns    | 250,744 ns         |
| seph-blog1      | 703,868 ns    | 589,501 ns         |

The relevant Rust rope libraries have benchmarks [here](https://github.com/josephg/jumprope-rs/blob/master/README.md#benchmarks) for reference.
