open Core
open Common
open Interpreters.Jargon_reg
open Common.Jargon_reg

let options : Options.t =
  { verbose_back= false
  ; verbose_front= false
  ; verbose_tree= false
  ; verbose_vm= true
  ; stack_max= 100
  ; heap_max= 100 }

let%expect_test "simple test" =
  let r1 = Register.fresh () in
  let r2 = Register.fresh () in
  let r3 = Register.fresh () in
  let instructions : Instruction.t list =
    [Set (r1, Int 10); Set (r2, Int 2); Oper (ADD, r3, (r1, r2)); Halt]
  in
  let _result = interpret options instructions in
  [%expect
    {|
    Installed Code =
    0: (Set r1(Int 10))
    1: (Set r2(Int 2))
    2: (Oper ADD r3(r1 r2))
    3: Halt
    ========== state 1 ==========
    cp = 0 -> (Set r1(Int 10))
    Stack = 0: (Frame_pointer 0)
    1: (Return_address 0)

    fp: (Frame_pointer 0)
    ra: (Return_address 0)
    ========== state 2 ==========
    cp = 1 -> (Set r2(Int 2))
    Stack = 0: (Frame_pointer 0)
    1: (Return_address 0)

    r1: (Int 10)
    fp: (Frame_pointer 0)
    ra: (Return_address 0)
    ========== state 3 ==========
    cp = 2 -> (Oper ADD r3(r1 r2))
    Stack = 0: (Frame_pointer 0)
    1: (Return_address 0)

    r1: (Int 10)
    r2: (Int 2)
    fp: (Frame_pointer 0)
    ra: (Return_address 0)
    ========== state 4 ==========
    cp = 3 -> Halt
    Stack = 0: (Frame_pointer 0)
    1: (Return_address 0)

    r1: (Int 10)
    r2: (Int 2)
    r3: (Int 12)
    fp: (Frame_pointer 0)
    ra: (Return_address 0)
    ========== state 5 ==========
    cp = 3 -> Halt
    Stack = 0: (Frame_pointer 0)
    1: (Return_address 0)

    r1: (Int 10)
    r2: (Int 2)
    r3: (Int 12)
    fp: (Frame_pointer 0)
    ra: (Return_address 0) |}]

let%expect_test "pair example" =
  let r1 = Register.fresh () in
  let r2 = Register.fresh () in
  let r3 = Register.fresh () in
  let r4 = Register.fresh () in
  let r5 = Register.fresh () in
  let r6 = Register.fresh () in
  let instructions : Instruction.t list =
    [ Set (r1, Int 10)
    ; Set (r2, Int 2)
    ; Make_pair (r3, (r2, r1))
    ; Fst (r4, r3)
    ; Snd (r5, r3)
    ; Oper (SUB, r6, (r4, r5))
    ; Halt ]
  in
  let _result = interpret options instructions in
  [%expect
    {|
    Installed Code =
    0: (Set r4(Int 10))
    1: (Set r5(Int 2))
    2: (Make_pair r6(r5 r4))
    3: (Fst r7 r6)
    4: (Snd r8 r6)
    5: (Oper SUB r9(r7 r8))
    6: Halt
    ========== state 1 ==========
    cp = 0 -> (Set r4(Int 10))
    Stack = 0: (Frame_pointer 0)
    1: (Return_address 0)

    fp: (Frame_pointer 0)
    ra: (Return_address 0)
    ========== state 2 ==========
    cp = 1 -> (Set r5(Int 2))
    Stack = 0: (Frame_pointer 0)
    1: (Return_address 0)

    r4: (Int 10)
    fp: (Frame_pointer 0)
    ra: (Return_address 0)
    ========== state 3 ==========
    cp = 2 -> (Make_pair r6(r5 r4))
    Stack = 0: (Frame_pointer 0)
    1: (Return_address 0)

    r4: (Int 10)
    r5: (Int 2)
    fp: (Frame_pointer 0)
    ra: (Return_address 0)
    ========== state 4 ==========
    cp = 3 -> (Fst r7 r6)
    Stack = 0: (Frame_pointer 0)
    1: (Return_address 0)

    Heap =
    0 -> (Header 3 Pair)
    1 -> (Int 2)
    2 -> (Int 10)
    r4: (Int 10)
    r5: (Int 2)
    r6: (Heap_index 0)
    fp: (Frame_pointer 0)
    ra: (Return_address 0)
    ========== state 5 ==========
    cp = 4 -> (Snd r8 r6)
    Stack = 0: (Frame_pointer 0)
    1: (Return_address 0)

    Heap =
    0 -> (Header 3 Pair)
    1 -> (Int 2)
    2 -> (Int 10)
    r4: (Int 10)
    r5: (Int 2)
    r6: (Heap_index 0)
    r7: (Int 2)
    fp: (Frame_pointer 0)
    ra: (Return_address 0)
    ========== state 6 ==========
    cp = 5 -> (Oper SUB r9(r7 r8))
    Stack = 0: (Frame_pointer 0)
    1: (Return_address 0)

    Heap =
    0 -> (Header 3 Pair)
    1 -> (Int 2)
    2 -> (Int 10)
    r4: (Int 10)
    r5: (Int 2)
    r6: (Heap_index 0)
    r7: (Int 2)
    r8: (Int 10)
    fp: (Frame_pointer 0)
    ra: (Return_address 0)
    ========== state 7 ==========
    cp = 6 -> Halt
    Stack = 0: (Frame_pointer 0)
    1: (Return_address 0)

    Heap =
    0 -> (Header 3 Pair)
    1 -> (Int 2)
    2 -> (Int 10)
    r4: (Int 10)
    r5: (Int 2)
    r6: (Heap_index 0)
    r7: (Int 2)
    r8: (Int 10)
    r9: (Int 8)
    fp: (Frame_pointer 0)
    ra: (Return_address 0)
    ========== state 8 ==========
    cp = 6 -> Halt
    Stack = 0: (Frame_pointer 0)
    1: (Return_address 0)

    Heap =
    0 -> (Header 3 Pair)
    1 -> (Int 2)
    2 -> (Int 10)
    r4: (Int 10)
    r5: (Int 2)
    r6: (Heap_index 0)
    r7: (Int 2)
    r8: (Int 10)
    r9: (Int 8)
    fp: (Frame_pointer 0)
    ra: (Return_address 0) |}]
