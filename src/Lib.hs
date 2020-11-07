{- Normalized polish expression [Wong and Liu, 1986]

# Problem

The original problem received a list of triplets:
  (A_i, r_i, s_i)  where A_i is the area
                         r_i and s_i are the limits of the aspect ratio

  Some modules are allowed to rotate 90 degrees.
  O_1 = fixed orientation modules
  O_2 = free orientation modules

  (1) w_i * h_i = A_i
  (2) r_i ≤ h_i/w_i ≤ s_i                                     if i ∋ O_1
  (3) r_i ≤ h_i/w_i ≤ s_i   or     1/r_i ≤ h_i/w_i ≤ s_i      if i ∋ O_2

Objective: minimize global bounding box subject to aspect ratio constraints
           minimize total wirelength between blocks
           a*area(F) + (1 - a)*L(F)     0 <= a <= 1

# Slicing floorplang, Slicing tree representation and Polish Expression

Polish expression: sequence of elements from {1,2,...,n,*,+} with the balloting property.
                   The traverse of a slicing tree in postorder gives a (proper) polish expression.

(i) Each block appears exactly once in the string
(ii) Balloting property: forall positions in the string #operators < #operand
(iii) no consecutive operator of the same type: normality property.

Normalized: no consecutive operators of the same type (H or V)
            1-1 correspondence between the set of normalized polished expressions of length 2n-1 and the set of
            skewed slicing trees with n leaves.

LIFO structure to convert polish expression to binary tree.

Moves:
 * M1: swap two adjacent operands
 * M2: complementing (swap H and V) some chain of operators
 * M3: swap a pair of adjacent operands and operators
! In  case of M3, we need to validate properties (ii) and (iii)

# Simulated Annealing


- Current = Best = 12*3*4*..*n*
- Cooling ratio = r = 0.85
- ∆_avg = perform random moves and compute the average value fo the magnitude of change in cost per move.
- T_0: we should have exp(-∆_avg/T_0) = P ~ 1, so T_0 = -∆_avg/ln(P)
- T_final = T_0 * 0.2 (~ 10 iterations, with r=0.85)
- T = T_0
- N = n*\gamma where \gamma is a user defined constant

repeat
     repeat
           New = Best + randomly select move // M3 requires trying several times.
           #moves <- #moves + 1
           ∆cost <- Cost(new) - Cost(Z)
           if (∆cost \leq 0) or (random(0,1) < e^{ -∆cost / T }) // Boltzmann acceptance criterion, where r is a random number [0, 1)
                if(∆cost < 0) then downhill <- downhill + 1
                Current = New
                if cost(Current) < cost(Best) then Best <- Current
           else
               reject <- reject + 1
     until (downhill >= N) or (#moves > 2N)
     T <- r*T
     #moves = 0
until (reject / #moves > 0.95) or (T <= T_final) or OutOfTime
return Best

# Our problem

To simplify our problem, we made some assumptions:
 * The blocks are rectangular and the shapes are given, at least one per module.
 * At most one connection between modules.

Problem:
  * adjacency list = [[3,4,5], [1,3,4,5,6], ...]
  * shape list = [[(3,5),(4,4)], [(2,6),(3,3)], ...]

-}
module Lib
  ( module Lib.PolishExpression
  , module Lib.SimulatedAnnealing
  ) where

import Lib.PolishExpression
import Lib.SimulatedAnnealing
