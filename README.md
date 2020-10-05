# Floorplanning

Objective Function: `min \alpha * area(F) + (1 - \alpha) * L(F)` where the parameter `0 \leq \alpha \leq 1` gives the relative importance between `area(F)` (bounding box) and `L(F)` (total wirelength). Determining the shapes of the blocks is also part of the problem.

### Input

Given a non-directed graph where each vertex represents a block and each edge a connection between the blocks.

The input format TBD.

For each block, a list of possible shapes (one or more)  is given.

For example, block A: (3,5), (4,4).

### Algorithm


1. Create an initial **polish expression** (normalize?)
2. Perturbate it.
3. Compute cost: area of the smallest rectangle + wiring length
  - The area needs to take into the best shape of blocks.
  - You need an algorithm for computing the area efficiently.
  - See slides.
4. Annealing Algorithm
