# TODO

- [ ] CLI: demo and run from files
- [ ] Avoid computing the cost of an already explored polish expression.
- [ ] Recompute shape curves only tore the shape on each branch and only recompute the branches (at most 2) that has changed after a perturbation on the polish expression.
- [ ] The modules shouldn't have a list of shapes. They should be rigit or flexible and allow rotations. There are better representations than `[Shape]`
- [ ] The representation of a floorplan is clunky. Most of the work of Floorplan.Pretty is transforming it to a matrix representation.

## IMPROVEMENTS

1. Shape curves as type, combine them with monoid.

2. Improve `PolishExpression`:

```haskell
data PEState = Normalized | Unnormalized
newtype PolishExpression (n :: PEState) = PolishExpression { _pe :: [Alphabet] }
```

3. Improve `SlicingTree`:

```haskell
data SlicingTree :: Type -> Type where
    Leaf :: ModuleIndex -> SlicingTree ModuleIndex
    Branch :: forall a. SlicingTree a -> Operator -> SlicingTree a -> SlicingTree Operator

deriving stock instance Show (SlicingTree a)
deriving stock instance (Eq a) => Eq (SlicingTree a)
```
