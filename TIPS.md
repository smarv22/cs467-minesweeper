# GHCi cheat sheet

Here's a little cheat sheet of REPL commands that will be helpful:

- Load a single module: `:l Minesweeper`
- Reload the current module: `:r`
- Quit the REPL: `:q`
- Print all information about a single identifier: `:i liftA2`
- Print the type of any expression: `:t liftA2 (,) idGrid exampleGrid1`
- Print the kind of any type: `:k StateT (Set Int)`


# Monadic operator cheat sheet

You can find the precedence (order of operations) for an operator in GHCi with ":i" - higher precedence comes first (check the precedence of * and + to see how it works). "infixr" means implicit parentheses group to the right, "infixl" means implicit parentheses group to the left, and "infix" means explicit parentheses are always required if the operator is used with other operators of the same precedence.

This cheat sheet is written from the perspective of working with monadic code ("effectful" values), but most of the operators only require Functor or Applicative instances. GHCi can tell you about this too.

To get the full set of these operators, you'll need the imports listed below. Again, GHCi can tell you where each of these comes from.

import Data.Functor
import Control.Applicative
import Control.Category
import Control.Monad

```
pure value            means a type like    a
effectful value       means a type like    m a
pure function         means a type like    a -> b
effectful function    means a type like    a -> m b
effectfully-curried   means a type like    m (a -> b)

pure function         $   pure value          (right-associative function application)
pure value            &   pure function       (flipped version of "$")
pure value           <$   effectful value     (execute right thing, return left thing)
effectful value       $>  pure value          (execute left thing, return right thing)
pure function        <$>  effectful value     (same as "fmap", apply pure function to effectful value)
effectful value      <*   effectful value     (execute both things left-to-right, return left thing)
effectful value       *>  effectful value     (execute both things left-to-right, return right thing)
effectfully-curried  <*>  effectful value     (execute both things left-to-right, call left thing on right thing)
effectful value      >>=  effectful function  (left arrow in "do"-notation)
effectful function   =<<  effectful value     ("concatMap" as an infix operator)
pure function        <<<  pure function       (same as ".", chain output of right function into left function)
pure function        >>>  pure function       (flipped ".", chain output of left function into right function)
effectful function   <=<  effectful function  (effectful ".", chain output of right function into left function)
effectful function   >=>  effectful function  (flipped "<=<", chain output of left function into right function)
```

# Project types cheat sheet

Here's a summary of the types that are relevant in this codebase, and what they mean in our overall Minesweeper implementation. See the comments in each module file for more detail.

## `0 :: Nat`, `1 :: Nat`, `2 :: Nat`, etc.

These are type-level natural numbers, which we use to track the dimensions of our Minesweeper board types in the type system.

The `KnownNat` constraints that show up in our code are pretty much just noise; the compiler will tell you where they're needed and where they're not needed.

## `Vector :: Nat -> Type -> Type`

This is like an array, or a fixed-size list with O(1) element access. Different-length `Vector`s have different types: a value of type `Vector 5 Int` must contain exactly 5 integers.

## `Finite :: Nat -> Type`

This is like an unsigned integer, but guaranteed to be below an upper bound specified in the type. The type `Finite 5` has exactly five values, `0`/`1`/`2`/`3`/`4`.

The main purpose of the `Finite` type is to provide guaranteed safe indexing into data structures like `Vector`, by using the type system to guarantee that the index is not out of bounds of the vector.

## `Grid :: Nat -> Nat -> Type -> Type`

This is a two-dimensional fixed-size matrix type represented as a `Vector` of `Vector`s. We use `Grid`s to represent the values on our Minesweeper board.

## `Index :: Nat -> Nat -> Type`

This is the index of a cell in a `Grid`. The purpose of `Index` is to provide guaranteed safe indexing into `Grid` values, like `Finite` does for `Vector`.

## `Field, Survey, Cover :: Nat -> Nat -> Type`

A Field is a Grid that represents where each mine is on the game board.

A Cover is a Grid that represents the visibility of each cell on the game board (whether the cell's value is hidden to the user in the UI).

A Survey is a Grid that represents the number shown on each cell on the game board when the cell is visible to the user.

We can easily generate a Survey from a Field.

Together, these three types contain all the data of the game board state.

## `SearchState :: Nat -> Nat -> Type`

This is the internal state that our "uncovering" algorithm uses to do its work.

## `State (SearchState w h) :: Type -> Type`

This is the monad that manages the `SearchState` in the depth-first search that drives the "uncovering" algorithm.

There are three fundamental commands in this monad:

- `put :: SearchState w h -> State (SearchState w h) ()` to set a new state value
- `get :: State (SearchState w h) (SearchState w h)` to access the current state value
- `modify :: (SearchState w h -> SearchState w h) -> State (SearchState w h) ()` to modify the current state value with the result of a function

## `StateT (Set a) IO :: Type -> Type`

This is the monad that manages the state and provides access to random values in the random mine placement algorithm.

There are four fundamental commands in this monad:

- `lift :: IO b -> StateT (Set a) IO b` to execute any `IO` action
- `put :: Set a -> StateT (Set a) IO ()` to set a new state value
- `get :: StateT (Set a) IO (Set a)` to access the current state value
- `modify :: (Set a -> Set a) -> StateT (Set a) IO ()` to modify the current state value with the result of a function

## `AppState :: Nat -> Nat -> Type`

This is the state that our UI uses to keep track of how to display and update the game board graphics.

## `Widget Void :: Type`

This is the type of Brick "widgets", or UI components. In this codeabse, each cell on the UI board is a `Widget` and the table that arranges the cells is also a `Widget`.

The `Void` part of `Widget Void` just says that we're not using a particular part of the Brick interface: specifically the widget caching system.

## `BrickEvent Void Void :: Type`

This is the type of Brick "events", which are generated when the external terminal state changes, including when the user presses any key.

Each time an event is generated, Brick calls our `handleEvent` function, which decides what to do in response to the event.

Both `Void` parts of `BrickEvent Void Void` just mean that we're opting out of Brick features, in this case both the widget caching system and the custom event system.

## `EventM Void (Next (AppState w h)) :: Type`

`EventM Void` is a monad, but in this codebase it only shows up in this one particular form as the return type of `HandleEvent`. There is no other use of the Next type in this codebase either.

This particular type is effectively a Boolean indicator of whether the UI should terminate or keep running, along with the value that will be the state of the UI in the next step.

There are two relevant commands that we use to create values of this type:

- `halt :: AppState w h -> EventM Void (Next (AppState w h))` takes a final `AppState` and exits the UI function
- `continue :: AppState w h -> EventM Void (Next (AppState w h))` takes a next `AppState` and continues the UI function

## `App (AppState w h) Void Void :: Type`

This is the type of our Brick "application", which is really just a collection of functions that Brick uses to build and run the UI.

In `main`, Brick's `defaultMain` function converts our `App` definition into a runnable `IO` function, which returns the final `AppState` value when it finishes.
