# TODO

- [ ] **Reuse previous computations on shape curves***: store the shape on each branch and only recompute the branches (at most 2) that has changed after a perturbation on the polish expression.
- [ ] The representation of a floorplan is clunky. Most of the work of Floorplan.Pretty is transforming it to a matrix representation.

## Improvements on the code

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
