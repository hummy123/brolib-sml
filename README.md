# Brolib-sml

## Introduction

Standard ML port of [this](https://github.com/hummy123/brolib) rope implementation.

This particular rope uses the balancing scheme described in the [Purely Functional 1-2 Brother Trees paper authored by Ralph Hinze](https://www.cs.ox.ac.uk/ralf.hinze/publications/Brother12.pdf). It tries to keep the number of nodes to a minimum by joining the strings in adjacent leaf nodes, if joining would not be too expensive.

## Usage

The two files are `rope.sml` and `tiny_rope.sml`. 

`rope.sml` contains a rope that tracks line metadata (which has a small performance and memory penalty). This is useful if you have line-based operations in mind.

`tiny_rope.sml` doesn't track line metadata, and is useful when line-queries aren't needed.

Except for those line-based operations marked below, all functions are the same between the two.

### Examples

#### Initialise

`val rope = Rope.fromString "hello, world!"`

It's best to use a string with a length less than or equal to 1024 for performance reasons. (The point of a rope is to represent a large string using a binary tree that contains smaller pieces.)

#### Convert to string

`val str = Rope.toString rope`

This is a function that is better to avoid.

#### Insert

`Rope.insert(0, "hello, world!")`
