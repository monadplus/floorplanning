# TODO

- [ ] CLI: demo and run from files
- [ ] Pass Demo/Run to simulatedAnneing. In run mode, there should be no delay
- [ ] Avoid computing the cost of an already explored polish expression.
- [ ] Recompute shape curves only tore the shape on each branch and only recompute the branches (at most 2) that has changed after a perturbation on the polish expression.
- [ ] The representation of a floorplan is clunky. Most of the work of Floorplan.Pretty is transforming it to a matrix representation.

## IMPROVEMENTS

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
