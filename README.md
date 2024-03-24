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

I don't know how to explain this result, but I assume most of the credit goes to the MLton compiler. It also seems likely that this is slower on string queries, as those Rust implementations use cache-friendly B-Trees as opposed to the binary tree used here.

(Note to self: worth giving numbers.)
