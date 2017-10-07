module Test.Main where

import Prelude

import Control.Monad.Eff (Eff, untilE)
import Control.Monad.Eff.AVar (AVAR, AVarStatus(Empty, Killed, Filled), killVar, makeEmptyVar, makeVar, putVar, readVar, status, takeVar, tryPutVar, tryReadVar, tryTakeVar)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (error)
import Control.Monad.Eff.Now (NOW, now)
import Control.Monad.Eff.Ref (REF, Ref, modifyRef, newRef, readRef)
import Data.DateTime.Instant (unInstant)
import Data.Either (Either(..))
import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..))
import Data.Newtype (wrap)
import Test.Assert (ASSERT, assert, assert')

type TestEff
  = Eff (avar ∷ AVAR, assert ∷ ASSERT, console ∷ CONSOLE, now ∷ NOW, ref ∷ REF)

assertEqual ∷ ∀ a. Eq a => Show a => { expected ∷ a, actual ∷ a } -> TestEff Unit
assertEqual {actual, expected} = do
  unless result $ log message
  assert' message result
  where
  message = "Expected: " <> show expected <> "\nActual:   " <> show actual
  result = actual == expected

assertEqualRef ∷ ∀ a. Eq a => Show a => a -> Ref a -> TestEff Unit
assertEqualRef expected ref = do
  actual <- readRef ref
  assertEqual { actual, expected }

assertEqualStatus
  ∷ ∀ a
  . Show a
  => { expected ∷ AVarStatus a, actual ∷ AVarStatus a }
  -> TestEff Unit
assertEqualStatus { actual, expected } =
  assertEqual { actual: show actual, expected: show expected }

assertTrue ∷ Boolean -> TestEff Unit
assertTrue actual = assertEqual { actual, expected: true }

assertFalse ∷ Boolean -> TestEff Unit
assertFalse actual = assertEqual { actual, expected: false }

waitForCallbacks ∷ Int -> Ref Int -> TestEff Unit
waitForCallbacks expected ref = do
  start <- now
  let checkCallbacks = (expected == _) <$> readRef ref
  untilE do
    actual <- readRef ref
    current <- now
    let timedOut = unInstant current - unInstant start > wrap 2000.0
    when (timedOut && expected /= actual) do
      log $ "Timed out. " <> show actual <> " of " <> show expected <> " callback(s) fired."
      assert false
    pure $ expected == actual

test ∷ ∀ a. String → TestEff a → TestEff Unit
test s k = k *> log ("[OK] " <> s)

test_tryRead_full ∷ TestEff Unit
test_tryRead_full = test "tryRead/full" do
  var ← makeVar "foo"
  val1 ← tryReadVar var
  val2 ← tryReadVar var
  assertEqual { actual: val1, expected: Just "foo" }
  assertEqual { actual: val2, expected: Just "foo" }

test_tryRead_empty ∷ TestEff Unit
test_tryRead_empty = test "tryRead/empty" do
  var ← makeEmptyVar
  val1 ∷ Maybe Unit ← tryReadVar var
  assertEqual { actual: val1, expected: Nothing }

test_tryPut_full ∷ TestEff Unit
test_tryPut_full = test "tryPut/full" do
  var ← makeVar "foo"
  res ← tryPutVar "bar" var
  assertFalse res

test_tryPut_empty ∷ TestEff Unit
test_tryPut_empty = test "tryPut/empty" do
  var ← makeEmptyVar
  res ← tryPutVar "foo" var
  val ← tryReadVar var
  assertTrue res
  assertEqual { actual: val, expected: Just "foo" }

test_tryTake_full ∷ TestEff Unit
test_tryTake_full = test "tryTake/full" do
  var ← makeVar "foo"
  res1 ← tryTakeVar var
  res2 ← tryTakeVar var
  assertEqual { actual: res1, expected: Just "foo" }
  assertEqual { actual: res2, expected: Nothing }

test_tryTake_empty ∷ TestEff Unit
test_tryTake_empty = test "tryTake/empty" do
  var  ← makeEmptyVar
  res1 ← tryTakeVar var
  res2 ← tryPutVar "foo" var
  res3 ← tryTakeVar var
  assertEqual { actual: res1, expected: Nothing }
  assertTrue res2
  assertEqual { actual: res3, expected: Just "foo" }

test_put_take ∷ TestEff Unit
test_put_take = test "put/take" do
  ref ← newRef 0
  var ← makeEmptyVar
  _ ← putVar "foo" var $ traverse_ \_ →
    modifyRef ref (_ + 1)
  _ ← takeVar var $ traverse_ \val →
    modifyRef ref (_ + 1)
  waitForCallbacks 2 ref

test_put_read_take ∷ TestEff Unit
test_put_read_take = test "put/read/take" do
  ref ← newRef 0
  var ← makeEmptyVar
  _ ← putVar "foo" var $ traverse_ \_ →
    modifyRef ref (_ + 1)
  _ ← readVar var $ traverse_ \val →
    modifyRef ref (_ + 1)
  _ ← takeVar var $ traverse_ \val →
    modifyRef ref (_ + 1)
  waitForCallbacks 3 ref

test_take_put ∷ TestEff Unit
test_take_put = test "take/put" do
  ref ← newRef 0
  var ← makeEmptyVar
  _ ← takeVar var $ traverse_ \val →
    modifyRef ref (_ + 1)
  _ ← putVar "foo" var $ traverse_ \_ →
    modifyRef ref (_ + 1)
  waitForCallbacks 2 ref

test_take_read_put ∷ TestEff Unit
test_take_read_put = test "take/read/put" do
  ref ← newRef 0
  var ← makeEmptyVar
  _ ← takeVar var $ traverse_ \val →
    modifyRef ref (_ + 1)
  _ ← readVar var $ traverse_ \val →
    modifyRef ref (_ + 1)
  _ ← putVar "foo" var $ traverse_ \_ →
    modifyRef ref (_ + 1)
  waitForCallbacks 3 ref

test_read_put_take ∷ TestEff Unit
test_read_put_take = test "read/put/take" do
  ref ← newRef 0
  var ← makeEmptyVar
  _ ← readVar var $ traverse_ \val →
    modifyRef ref (_ + 1)
  _ ← putVar "foo" var $ traverse_ \_ →
    modifyRef ref (_ + 1)
  _ ← takeVar var $ traverse_ \val → do
    modifyRef ref (_ + 1)
  waitForCallbacks 3 ref

test_read_take_put ∷ TestEff Unit
test_read_take_put = test "read/take/put" do
  ref ← newRef 0
  var ← makeEmptyVar
  _ ← readVar var $ traverse_ \val → do
    modifyRef ref (_ + 1)
    void $ takeVar var $ traverse_ \val' →
      modifyRef ref (_ + 1)
  _ ← putVar "foo" var $ traverse_ \_ →
    modifyRef ref (_ + 1)
  waitForCallbacks 3 ref

test_kill_full ∷ TestEff Unit
test_kill_full = test "kill/full" do
  ref ← newRef 0
  var ← makeEmptyVar
  _ ← putVar "foo" var $ traverse_ \_ →
    modifyRef ref (_ + 1)
  killVar (error "Die.") var
  _ ← readVar var case _ of
    Left err → modifyRef ref (_ + 1)
    Right _  → modifyRef ref (_ + 2)
  waitForCallbacks 2 ref

test_kill_empty ∷ TestEff Unit
test_kill_empty = test "kill/empty" do
  ref ← newRef 0
  var ← makeEmptyVar
  killVar (error "Die.") var
  _ ← readVar var case _ of
    Left err → modifyRef ref (_ + 1)
    Right _  → modifyRef ref (_ + 2)
  waitForCallbacks 1 ref

test_kill_pending ∷ TestEff Unit
test_kill_pending = test "kill/pending" do
  ref ← newRef 0
  var ← makeEmptyVar
  let
    cb s = case _ of
      Left err → modifyRef ref (_ + 1)
      Right _  → modifyRef ref (_ + 1)
  _ ← takeVar var (cb "a")
  _ ← takeVar var (cb "b")
  _ ← readVar var (cb "c")
  _ ← readVar var (cb "d")
  killVar (error "-die.") var
  waitForCallbacks 4 ref

test_cancel ∷ TestEff Unit
test_cancel = test "cancel" do
  ref ← newRef 0
  v1 ← makeVar ""
  c1 ← putVar "a" v1 $ traverse_ \_ → modifyRef ref (_ + 1)
  c2 ← putVar "b" v1 $ traverse_ \_ → modifyRef ref (_ + 1)
  c3 ← putVar "c" v1 $ traverse_ \_ → modifyRef ref (_ + 1)
  c1
  c2
  _  ← tryTakeVar v1
  _  ← tryTakeVar v1
  _  ← tryTakeVar v1
  v2 ← makeEmptyVar
  c4 ← takeVar v2 $ traverse_ \_ → modifyRef ref (_ + 1)
  c5 ← takeVar v2 $ traverse_ \_ → modifyRef ref (_ + 1)
  c6 ← takeVar v2 $ traverse_ \_ → modifyRef ref (_ + 1)
  c5
  _  ← tryPutVar "a" v2
  _  ← tryPutVar "b" v2
  _  ← tryPutVar "c" v2
  v3 ← makeEmptyVar
  c7 ← readVar v3 $ traverse_ \_ → modifyRef ref (_ + 1)
  c8 ← readVar v3 $ traverse_ \_ → modifyRef ref (_ + 1)
  c9 ← readVar v3 $ traverse_ \_ → modifyRef ref (_ + 1)
  c8
  c9
  _  ← tryPutVar "a" v3
  waitForCallbacks 4 ref

test_status_full ∷ TestEff Unit
test_status_full = test "status/full" do
  var ← makeVar "a"
  s ← status var
  assertEqualStatus { actual: s, expected: Filled "a" }

test_status_kill ∷ TestEff Unit
test_status_kill = test "status/kill" do
  var ← makeVar "a"
  killVar (error "b") var
  s ← status var
  assertEqualStatus { actual: s, expected: Killed (error "b") }

test_status_empty ∷ TestEff Unit
test_status_empty = test "status/empty" do
  var ← makeVar "a"
  _ <- takeVar var \_ -> pure unit
  s ← status var
  assertEqualStatus { actual: s, expected: Empty }

main ∷ TestEff Unit
main = do
  test_tryRead_full
  test_tryRead_empty
  test_tryPut_full
  test_tryPut_empty
  test_tryTake_full
  test_tryTake_empty
  test_put_take
  test_take_put
  test_take_read_put
  test_read_put_take
  test_read_take_put
  test_kill_full
  test_kill_empty
  test_kill_pending
  test_cancel
  test_status_full
  test_status_kill
  test_status_empty
