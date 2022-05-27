-- This module defines a polymorphic Grid type that we'll use as the basis of
-- our game board types in the Minesweeper game. To make things extra
-- interesting, our Grid type will also track its dimensions in its type: a
-- Grid of size 3x2 is a different type than a Grid of size 5x3, for example.

-- Admittedly, I've mainly chosen to design the type this way because I think
-- it'll be a good exercise to work with it; this is not necessarily the *best*
-- way to design a purely functional Minesweeper program. It does turn out to
-- be a pretty decent way, at least.

-- To motivate the exercise, though, here's the motivation for why we might
-- want to track the dimensions of a collection at the type level: we have a
-- guarantee that we'll never hit an out-of-bounds error at runtime!

-- This protects us from a whole class of errors, because our bounds checking
-- is being done at compile-time instead of at runtime. Does this mean we can
-- safely eliminate runtime bounds checks from our compiled program altogether?

-- Sort of! We're never going to fully eliminate bounds checking if our bounds
-- are affected by user input, because users are really hard to prove things
-- about. What we can do is *track* the bounds checking that we do, in order to
-- ensure that we never need to do *redundant* bounds checking at runtime.

-- It turns out that the type system is a pretty good place to do this kind of
-- tracking. Haskell's type system is able to track natural number values
-- (non-negative integers including zero) at the type-level, which is what
-- we'll be exploring a bit in this code.

-- Our code will have no possibility of an out-of-bounds error, with no runtime 
-- able to track exactly where bounds-checking *is* happening through the types
-- of our functions. Again, this is way overkill for Minesweeper, but it's also
-- a good exploration of some nuances of programming with a kind system.

module Grid where

-- There are comments on most of these things at the points where they come up
-- in the code, so just ignore this chunk of imports for now and come back to
-- it when you're wondering where a particular name comes from.
import Control.Applicative (liftA2)
import Data.Finite (Finite, shift, unshift, weaken, strengthen)
import Data.Foldable (toList)
import Data.Functor.Compose (Compose(..))
import Data.Kind (Type, Constraint)
import Data.Maybe (catMaybes)
import Data.Proxy (Proxy(..))
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Data.Traversable (for)
import Data.Vector.Sized (Vector)
import Data.Vector.Sized qualified as Vector
import GHC.TypeNats (Nat, KnownNat)


-- From GHC.TypeNats, we have a *kind* called Nat, and *types* of kind Nat,
-- whicw he write as regular old numbers, like 0 or 5 or 123. The Nat kind and
-- its types represent type-level natural numbers (non-negative integers
-- including zero).

-- This is a little weird, so give yourself time to process it, and remind
-- yourself of exactly what *kind* and *type* mean here.

-- Type-level numbers are really pretty much the same as value-level numbers,
-- but they exist in memory while the typechecker is doing its work, not while
-- the compiled program is being run.

-- When we actually want or need to do runtime bounds checking, a type-level
-- natural number can be "demoted" to a runtime value. This can feel like
-- magic, but the trick is just that the typechecker is able to include some
-- additional constant definitions in the compiled output code, which all
-- naturally happens before runtime.

-- This will (hopefully) start to make more sense through some examples of how
-- we use type-level natural numbers in actual program code.


-- The (badly-named) Finite type is a type of bounded natural numbers. Check
-- the kind of Finite in the REPL: here it is for reference.

--   Finite :: Nat -> Type

-- What does this mean?

-- Remember our rule for the -> symbol in both types and kinds:

-- if
--   f :: A -> B
-- and
--   x :: A
-- then
--   f x :: B

-- We have, for example:

-- 1 :: Nat
-- Finite :: Nat -> Type
-- Finite 1 :: Type

-- This means that "Finite 1" is a type which describes a set of values, just
-- like Int or String.

-- Specifically, Finite 1 is the type of all natural numbers *less than* 1.
-- This type has exactly one value, which is the number 0.

--   0 :: Finite 1

-- We are mixing value-level numbers and type-level numbers here, so be careful
-- to note the difference.

--   1 :: Nat      -- the *type* 1 has *kind* Nat
--   0 :: Finite 1 -- the *value* 0 has *type* Finite 1

-- The purpose of the Nat kind is to allow us to write a combination like
-- Finite 1 while also ruling out a combination like Finite String, which hould
-- be nonsense with this definition of the Finite type.

-- Finite 0 is an empty type, like Void; Finite 5 is a type containing only 0,
-- 1, 2, 3, and 4; and so on.


-- Disappointingly, Haskell will not actually stop us from writing
--   5 :: Finite 3
-- even though 5 should not be a member of the type Finite 3 by definition.

-- This will cause a runtime error, which means each time we write a Finite
-- literal this way, it is actually bounds-checked once at runtime. This means
-- we are still safe from segfaults, but not fully safe from our own careless
-- mistakes.

-- This is obviously not ideal. If we want to handle the possibility of that
-- error at runtime, or if we want to take in user-provided numbers and
-- explicitly bounds check them, we can use the packFinite function of
-- Data.Finite, which returns a Maybe (Finite n):

--   (packFinite 3 :: Finite 5) == Just 3
--   (packFinite 5 :: Finite 3) == Nothing

-- But this is kind of silly if we are using numeric literals that are
-- hard-coded at compile-time, right? We shouldn't have to wait until runtime
-- to discover that the constant 5 is not less than the constant 3.

-- There is a way to write Finite literals that are bounds-checked at compile
-- time, but it's a little awkward because it's based on an old idiom.

--   natToFinite @3 Proxy :: Finite 5

-- The Proxy argument has to be there, but it doesn't really serve any purpose,
-- and we can generally expect it to get optimized away to nothing at runtime.
-- (It's vestigial, from before we could use the @3 notation.)

-- If we try to write an invalid Finite value with natToFinite, we get a
-- *compile-time* error:

--   natToFinite @5 Proxy :: Finite 3
--       • Couldn't match type ‘'False’ with ‘'True’
--         arising from a use of ‘natToFinite’

-- The text of the error is useless as usual, but the existence of the error
-- means that we're safe from typos incurring segfaults, with no need for even
-- a single runtime bounds check.


-- The dec and inc functions below are the only two Finite functions we'll have
-- to define ourselves; the rest will come from our vector library.

-- Don't worry about understanding how these function implementations work,
-- although you're welcome to ask about them. You won't really need to think
-- about the internal structure of the Finite type in order to do anything else
-- with this codebase.

-- dec is our "try decrementing" function: if the input is 0 it returns
-- Nothing, and otherwise it subtracts 1.

-- inc is our "try incrementing" function: if the input is the maximum element
-- of its type it returns Nothing, and otherwise it adds 1.

-- (For example, 4 is the maximum element of Finite 5.)

-- We don't have to write (n :: Nat), since Haskell can infer it from our usage
-- of n. We could just write "forall n.", or we could leave out the entire
-- "forall" clause.
dec :: forall (n :: Nat). Finite n -> Maybe (Finite n)
dec = unshift . weaken

inc :: KnownNat n => Finite n -> Maybe (Finite n)
inc = strengthen . shift

-- Why does this KnownNat constraint show up in inc but not in dec?

-- The presence of the KnownNat constraint indicates that n is being demoted to
-- runtime, and it may be used in a runtime bounds check. If there is no
-- KnownNat constraint, we have a guarantee that there is no runtime bounds
-- check *using n*.

-- The reason dec doesn't need a KnownNat constraint is because it's checking
-- against 0 as a lower bound, not against n. In dec, n does not need to be
-- demoted to a runtime value; in inc, it does.

-- In theory, this means the absence of a KnownNat constraint isn't actually a
-- guarantee that no runtime bounds check is being performed. The presence of a
-- KnownNat constraint *is* a guarantee that the number will be stored in
-- memory at runtime (unless it's optimized away), but it's also not actually a
-- guarantee that a bounds check will happen.

-- In practice, this means KnownNat constraints mean almost nothing of interest
-- to us as programmers, and they're basically just an annoying consequence of
-- the way Haskell handles type-level numbers. They'll show up all over our
-- code and you can really just ignore them.

-- If you need a KnownNat constraint, the compiler will actually provide a
-- somewhat helpful error:

--   inc :: forall n. Finite n -> Maybe (Finite n)
--   inc = strengthen . shift

--    • No instance for (KnownNat n) arising from a use of ‘strengthen’
--      Possible fix:
--        add (KnownNat n) to the context of
--          the type signature for:
--            inc :: forall (n :: Nat). Finite n -> Maybe (Finite n)

-- Languages like Idris and Agda handle this better; the feature Haskell is
-- missing is full dependent types. The world of values and the world of types
-- are separate, so we need some construct in the language to explicitly
-- connect those worlds, and for natural numbers that construct is KnownNat.


-- Anyway! That was a lot of preamble, but let's get on to defining our Grid
-- type. The reason for all the discussion of Finite is that we're going to use
-- Finite values as *indices* for our Grid type, instead of something like the
-- Int type. This is how we'll guarantee against out-of-bounds accesses.

-- Check the kind of Grid in the REPL: w and h are both Nats, and a is any
-- Type. The w parameter is the width of the grid and the h parameter is the
-- height of the grid.
newtype Grid w h a where
  -- The Vector type is an array-like type with a fixed size, with its length
  -- tracked in the type system.
  Grid ::
    { gridCells :: Vector h (Vector w a)
    } -> Grid w h a
  deriving
    ( Eq, Ord, Show
    , Functor, Foldable, Traversable
    )
  deriving
    Applicative via Compose (Vector h) (Vector w)

-- We get a ton of code for free with the deriving clauses above, including an
-- Applicative instance through the DerivingVia extension. I'll leave it as an
-- optional exercise for the reader to search about the DerivingVia trick, but
-- below we'll look at how this Applicative instance can be used.

-- First, how do we create a Grid?

exampleGrid1 :: Grid 3 5 Int
exampleGrid1 =
  Grid
    (Vector.fromTuple
      ( Vector.fromTuple (1, 2, 3)
      , Vector.fromTuple (4, 5, 6)
      , Vector.fromTuple (7, 8, 9)
      , Vector.fromTuple (10, 11, 12)
      , Vector.fromTuple (13, 14, 15)
      ))

-- Note how you'll get a compile-time error if you use Vector.fromTuple with a
-- different number of elements than you declared in the type of the grid.

-- Grid values will display badly in the REPL by default; below, there's a
-- printGrid function defined that will let you see the grid as a grid in your
-- REPL.

--   printGrid exampleGrid1

-- Remember, the $ operator can be used to avoid right-nested parentheses. Be
-- careful with it: if you ever have an error in code using the $ operator, the
-- first thing to do is to rewrite it with parentheses instead.

exampleGrid2 :: Grid 4 2 Bool
exampleGrid2 =
  Grid $
    Vector.fromTuple
      ( Vector.fromTuple (True, False, False, True)
      , Vector.fromTuple (True, True, False, True)
      )

-- The details of exactly where the $ operator inserts its "invisible
-- parentheses" are subtle, but the general principle is "from here to the end
-- of the expression". Keep in mind that in a do block, the end of the
-- expression may not be the end of the whole block.

-- Now, what do our typeclass instances for Grid do for us?

-- The illustrations below are for w = h = 3, but the principles apply to Grid
-- values of any dimensions. These illustrations are not quite valid Haskell
-- syntax, but hopefully it's clear how they correspond to the actual grid
-- definition syntax like in the examples above.

-- fmap applies a function to each element of a grid:

--          (x11, x21, x31)   (f x11, f x21, f x31)
--   fmap f (x12, x22, x32) = (f x12, f x22, f x32)
--          (x13, x23, x33)   (f x13, f x23, f x33)

-- pure produces a grid where every cell has the same element:

--            (x, x, x)
--   pure x = (x, x, x)
--            (x, x, x)

-- (<*>) applies a grid of functions to a grid of values:

--   (f11, f21, f31)     (x11, x21, x31)   (f11 x11, f21 x21, f31 x31)
--   (f12, f22, f32) <*> (x12, x22, x32) = (f12 x12, f22 x22, f32 x32)
--   (f13, f23, f33)     (x13, x23, x33)   (f13 x13, f23 x23, f33 x33)

-- liftA2 applies a function to cells in the same position across two grids:

--            (x11, x21, x31) (y11, y21, y31)   (f11 x11 y11, f21 x21 y21, f31 x31 y31)
--   liftA2 f (x12, x22, x32) (y12, y22, y32) = (f12 x12 y12, f22 x22 y22, f32 x32 y32)
--            (x13, x23, x33) (y13, y23, y33)   (f13 x13 y13, f23 x23 y23, f33 x33 y33)

-- The Foldable instance essentially treats the Grid as a flat list of cells,
-- which is occasionally convenient for checking whether any or all cells
-- satisfy a particular condition, for example:

--            (1, 2, 3)
--   any even (4, 5, 6) = any even [1,2,3,4,5,6,7,8,9] = True
--            (7, 8, 9)

-- The Traversable instance isn't actually used in this codebase, but the
-- traverse function provides some fun opportunities with the Grid type if you
-- want to explore it.


-- Okay! That's all the long-form intro for the Grid type. The rest of this
-- module will just have documentation for each definition and how it fits into
-- our overall scheme.


-- Converts a Grid to a list of lists, which is a similar structure with
-- less guarantees. This is convenient for built-in functions that expect lists
-- instead of Vectors.
gridToLists :: Grid w h a -> [[a]]
gridToLists = toList . fmap toList . gridCells

-- Converts a Grid to a Text value separating the cells with spaces and
-- newlines. Use with Text.putStr, or use printGrid.
prettyGrid ::
  forall w h a.
  Show a =>
  Grid w h a -> Text
prettyGrid g =
  Text.unlines $ Vector.toList $
    fmap (Text.unwords . Vector.toList) $
      gridCells $ fmap (Text.pack . show) g

-- Prints a Grid to standard console output separating the cells with spaces
-- and newlines.
printGrid ::
  forall w h a.
  Show a =>
  Grid w h a -> IO ()
printGrid = Text.putStr . prettyGrid


-- The type of indices in a Grid: a pair of numbers (c,r), where c is the
-- column index and r is the row index. We have a type-level guarantee that
-- 0 <= c < w and 0 <= r < h.
type Index w h = (Finite w, Finite h)

-- Since these two KnownNat constraints will show up together often in our
-- code, we bundle them into a constraint synonym. Check the kind of this in
-- the REPL: it has kind Constraint, which means it can be used on the left of
-- a => just like a typeclass. This is really just shorthand: everywhere we use
-- KnownBounds w h, we could replace it with (KnownNat w, KnownNat h).
type KnownBounds w h = (KnownNat w, KnownNat h)


-- Access an individual cell in a Grid. This doesn't do any runtime
-- bounds-checking, because Vector.index trusts the typechecker to enforce the
-- bounds of the Finite type.
index ::
  forall w h a.
  Grid w h a -> Index w h -> a
index g (r,c) = Vector.index (Vector.index (gridCells g) c) r

-- Generate a Grid by giving a function that maps each possible Index in bounds
-- to a value. This is a convenient way to populate a grid, as we'll see.
generate ::
  forall w h a.
  KnownBounds w h =>
  (Index w h -> a) -> Grid w h a
generate f = Grid (Vector.generate (\r -> Vector.generate (\c -> f (c,r))))

-- A grid where each cell contains its own index. This is also convenient,
-- especially with liftA2/liftA3/(<*>).
idGrid ::
  forall w h a.
  KnownBounds w h =>
  Grid w h (Index w h)
idGrid = generate id

-- Map over each element of a grid along with its index. Another common name
-- for this pattern is "mapWithIndex".
imap ::
  forall w h a b.
  KnownBounds w h =>
  (Index w h -> a -> b) -> Grid w h a -> Grid w h b
imap f = liftA2 f idGrid

-- Replace an element at a specific Index in a Grid.
replace ::
  forall w h a.
  KnownBounds w h =>
  Index w h -> a -> Grid w h a -> Grid w h a
replace i x = imap (\j y -> if i == j then x else y)

-- Get a list of every cell adjacent to a given Index in a Grid. This returns a
-- list instead of a Vector because it may have anywhere from 3 to 8 values
-- (the corners of a Grid each have only 3 adjacent cells).
neighborhood ::
  forall w h a.
  KnownBounds w h =>
  Grid w h a -> Index w h -> [a]
neighborhood g (r,c) =
  fmap (index g) $ catMaybes $ fmap (uncurry (liftA2 (,)))
    [ (dec  r, dec c), (dec r, Just c), (dec  r, inc c)
    , (Just r, dec c),                  (Just r, inc c)
    , (inc  r, dec c), (inc r, Just c), (inc  r, inc c)
    ]

-- Replace all of cells in the neighborhood of an index with the same one
-- value. This is convenient in our Minesweeper search algorithm.
replaceNeighborhood ::
  forall w h f a.
  KnownBounds w h =>
  Index w h -> a -> Grid w h a -> Grid w h a
replaceNeighborhood i x =
  imap (\j y -> if elem j (neighborIndices i) then x else y)

-- Get a list of indices that are adjacent to a given Index.
neighborIndices ::
  forall w h a.
  KnownBounds w h =>
  Index w h -> [Index w h]
neighborIndices = neighborhood idGrid

-- Construct a Grid where each cell contains the neighborhood of the
-- corresponding cell in the input Grid. Try this out with small grids in the
-- REPL to see how it works. This is our Minesweeper mine counting algorithm.
neighborhoods ::
  forall w h a.
  KnownBounds w h =>
  Grid w h a -> Grid w h [a]
neighborhoods g = fmap (neighborhood g) idGrid
