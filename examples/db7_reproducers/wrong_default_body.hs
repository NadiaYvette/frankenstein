module WrongDefaultBody where

-- DB7 minimum reproducer: self-host stage 2 emits the FIRST conBranch's
-- body in place of the default `_` body when classifyBranches encounters
-- an exhaustive ADT case with an explicit default.
--
-- Source: case x1 of
--   C1_1 v1 -> v1 + v1 + v1     -- 3 v1 refs
--   C1_2 v1 -> v1 + v1          -- 2 v1 refs
--   _       -> 88 + 86          -- 0 v1 refs (= 174)
--
-- Host emit: classifyBranches deduces exhaustivity, drops the default,
-- compiles to 2-branch scf.if; runtime returns 207 (= 69 * 3).
--
-- Self-host emit: keeps the default but its body becomes branch 1's
-- (v1+v1+v1) referencing v1 from C1_1 which is out of scope at the
-- default site, producing `Repro_v1zd...$0()` external calls that
-- the linker can't resolve.  Symptom: clang link error
--   undefined reference to `WrongDefaultBody_v1zd...$0`
--
-- Expected runtime: 207

data T1 = C1_1 Int | C1_2 Int

f2 :: T1 -> Int
f2 x1 = case x1 of
  C1_1 v1 -> (v1 + v1) + v1
  C1_2 v1 -> v1 + v1
  _       -> 88 + 86

main :: Int
main = f2 (C1_1 69)
