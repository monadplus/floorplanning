# FIXME

```
Missing module

┌               ┐
│ 1 1 1 1 8 8 0 │
│ 0 0 0 0 8 9 9 │
│ 7 7 7 7 8 9 9 │
│ 6 6 6 6 8 8 4 │
│ 5 5 5 5 8 8 4 │
│ 5 5 5 5 8 8 4 │
└               ┘

Overlapping

-- ┌                   ┐
-- │ 4 4 4 6 6 1 1 1 1 │
-- │ 4 4 4 6 6 2 2 2 7 │
-- │ 5 5 5 6 6 2 2 2 7 │
-- │ 5 5 5 6 6 2 2 2 7 │
-- │ 8 8 8 8 8 8 9 9 7 │
-- │ 8 8 8 8 8 8 9 9 7 │
-- └                   ┘

-- 4:     0 4   3 6
-- 5:     0 2   4 4
-- 6:     3 2   5 6
```

# TODO

- [ ] CLI: demo and run from files
- [ ] Avoid computing the cost of an already explored polish expression.
- [ ] Recompute shape curves only tore the shape on each branch and only recompute the branches (at most 2) that has changed after a perturbation on the polish expression.
- [ ] The modules shouldn't have a list of shapes. They should be rigit or flexible and allow rotations.
      There are better representations than `[Shape]`
- [ ] The representation of a floorplan is clunky. Most of the work of Floorplan.Pretty is transforming it to a matrix representation.

## IMPROVEMENTS

- Shape curves as type, combine them with monoid.

- Improve `PolishExpression`:

```haskell
data PEState = Normalized | Unnormalized
newtype PolishExpression (n :: PEState) = PolishExpression { _pe :: [Alphabet] }
```

- Improve `SlicingTree`:

```haskell
data SlicingTree :: Type -> Type where
    Leaf :: ModuleIndex -> SlicingTree ModuleIndex
    Branch :: forall a. SlicingTree a -> Operator -> SlicingTree a -> SlicingTree Operator

deriving stock instance Show (SlicingTree a)
deriving stock instance (Eq a) => Eq (SlicingTree a)
```
