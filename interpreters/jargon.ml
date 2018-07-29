open Common
open Common.Jargon

let stack_to_heap_item = function
  | STACK_INT i -> HEAP_INT i
  | STACK_BOOL b -> HEAP_BOOL b
  | STACK_UNIT -> HEAP_UNIT
  | STACK_HI i -> HEAP_HI i
  | STACK_RA i -> HEAP_CI i
  | STACK_FP _i ->
      Errors.complain "stack_to_heap_item: no frame pointer allowed on heap"

let heap_to_stack_item = function
  | HEAP_INT i -> STACK_INT i
  | HEAP_BOOL b -> STACK_BOOL b
  | HEAP_UNIT -> STACK_UNIT
  | HEAP_HI i -> STACK_HI i
  | HEAP_CI i -> STACK_RA i
  | HEAP_HEADER (_, _) ->
      Errors.complain "heap_to_stack_item : heap header not allowed on stack"

type status_code =
  | Halted
  | Running
  | CodeIndexOutOfBound
  | StackIndexOutOfBound
  | HeapIndexOutOfBound
  | StackUnderflow

type vm_state =
  { stack_bound: stack_index
  ; code_bound: code_index
  ; heap_bound: code_index
  ; stack: stack_item array
  ; heap: heap_item array
  ; code: instruction array
  ; mutable sp: stack_index
  ; (* stack pointer *)
    mutable fp: stack_index
  ; (* frame pointer *)
    mutable cp: code_index
  ; (* code pointer  *)
    mutable hp: heap_index
  ; (* next free     *)
    mutable status: status_code }

let get_instruction (vm: vm_state) = (vm.code).(vm.cp)

let stack_top (vm: vm_state) = (vm.stack).(vm.sp - 1)


let string_of_status = function
  | Halted -> "halted"
  | Running -> "running"
  | CodeIndexOutOfBound -> "code index out-of-bound"
  | StackIndexOutOfBound -> "stack index out-of-bound"
  | HeapIndexOutOfBound -> "heap index out-of-bound"
  | StackUnderflow -> "stack underflow"

let string_of_heap vm =
  let rec aux k =
    if vm.hp <= k then ""
    else
      string_of_int k ^ " -> "
      ^ string_of_heap_item (vm.heap).(k)
      ^ "\n"
      ^ aux (k + 1)
  in
  "\nHeap = \n" ^ aux 0

let string_of_state vm =
  "cp = " ^ string_of_int vm.cp ^ " -> "
  ^ string_of_instruction (get_instruction vm)
  ^ "\n" ^ "fp = " ^ string_of_int vm.fp ^ "\n" ^ "Stack = \n"
  ^ string_of_stack (vm.sp, vm.stack)
  ^ if vm.hp = 0 then "" else string_of_heap vm


(* the following two functions are needed to
   pretty-print heap and stack values
 *)
let rec string_of_heap_value a vm =
  match (vm.heap).(a) with
  | HEAP_INT i -> string_of_int i
  | HEAP_BOOL true -> "true"
  | HEAP_BOOL false -> "false"
  | HEAP_UNIT -> "()"
  | HEAP_HI i -> string_of_heap_value i vm
  | HEAP_CI _ ->
      Errors.complain
        "string_of_heap_value: expecting value in heap, found code index"
  | HEAP_HEADER (_i, ht) ->
    match ht with
    | HT_PAIR ->
        "("
        ^ string_of_heap_value (a + 1) vm
        ^ ", "
        ^ string_of_heap_value (a + 2) vm
        ^ ")"
    | HT_INL -> "inl(" ^ string_of_heap_value (a + 1) vm ^ ")"
    | HT_INR -> "inr(" ^ string_of_heap_value (a + 1) vm ^ ")"
    | HT_CLOSURE -> "CLOSURE"

let string_of_value vm =
  match stack_top vm with
  | STACK_INT i -> string_of_int i
  | STACK_BOOL true -> "true"
  | STACK_BOOL false -> "false"
  | STACK_UNIT -> "()"
  | STACK_HI a -> string_of_heap_value a vm
  | STACK_RA _ ->
      Errors.complain
        "string_of_value: expecting value on stack top, found code index"
  | STACK_FP _ ->
      Errors.complain
        "string_of_value: expecting value on stack top, found frame pointer"



(* cp := cp + 1  *)
let advance_cp vm =
  if vm.cp < vm.code_bound then {vm with cp= vm.cp + 1}
  else {vm with status= CodeIndexOutOfBound}

let goto (i, vm) = {vm with cp= i}

(* pop n items from stack *)
let pop (n, vm) =
  if 0 <= vm.sp - n then {vm with sp= vm.sp - n}
  else {vm with status= StackUnderflow}

let pop_top vm =
  let c = stack_top vm in
  (c, pop (1, vm))

(* pop c onto stack  *)
let push (c, vm) =
  if vm.sp < vm.stack_bound then
    let _ = (vm.stack).(vm.sp) <- c in
    {vm with sp= vm.sp + 1}
  else {vm with status= StackIndexOutOfBound}

(* let swap vm =
 *   let c1, vm1 = pop_top vm in
 *   let c2, vm2 = pop_top vm1 in
 *   push (c2, push (c1, vm2)) *)

let do_unary = function
  | Oper.NOT, STACK_BOOL m -> STACK_BOOL (not m)
  | NEG, STACK_INT m -> STACK_INT (-m)
  | READ, STACK_UNIT -> STACK_INT (Utils.read_int ())
  | op, _ ->
      Errors.complain
        ("do_unary: malformed unary operator: " ^ Oper.string_of_uop op)

let do_oper = function
  | Oper.AND, STACK_BOOL m, STACK_BOOL n -> STACK_BOOL (m && n)
  | OR, STACK_BOOL m, STACK_BOOL n -> STACK_BOOL (m || n)
  | EQB, STACK_BOOL m, STACK_BOOL n -> STACK_BOOL (m = n)
  | LT, STACK_INT m, STACK_INT n -> STACK_BOOL (m < n)
  | EQI, STACK_INT m, STACK_INT n -> STACK_BOOL (m = n)
  | ADD, STACK_INT m, STACK_INT n -> STACK_INT (m + n)
  | SUB, STACK_INT m, STACK_INT n -> STACK_INT (m - n)
  | MUL, STACK_INT m, STACK_INT n -> STACK_INT (m * n)
  | DIV, STACK_INT m, STACK_INT n -> STACK_INT (m / n)
  | op, _, _ ->
      Errors.complain
        ("do_oper: malformed binary operator: " ^ Oper.string_of_bop op)

let perform_op (op, vm) =
  let v_right, vm1 = pop_top vm in
  let v_left, vm2 = pop_top vm1 in
  push (do_oper (op, v_left, v_right), vm2)

let perform_unary (op, vm) =
  let v, vm1 = pop_top vm in
  push (do_unary (op, v), vm1)

(* implement garbage collection!

   This should free up all heap space
   not reachable from the stack.

   Might also increase heap size.

   Result:
   None = no progress
   Some(vm') = progress made, resulting in vm'
*)
let invoke_garbage_collection _vm = None

let allocate (n, vm) =
  let hp1 = vm.hp in
  if hp1 + n < vm.heap_bound then (hp1, {vm with hp= vm.hp + n})
  else
    match invoke_garbage_collection vm with
    | None -> Errors.complain "allocate : heap exhausted"
    | Some vm2 ->
        if vm2.hp + n < vm2.heap_bound then (vm2.hp, {vm2 with hp= vm2.hp + n})
        else Errors.complain "allocate : heap exhausted"

let mk_pair vm =
  let v_right, vm1 = pop_top vm in
  let v_left, vm2 = pop_top vm1 in
  let a, vm3 = allocate (3, vm2) in
  let header = HEAP_HEADER (3, HT_PAIR) in
  let _ = (vm.heap).(a) <- header in
  let _ = (vm.heap).(a + 1) <- stack_to_heap_item v_left in
  let _ = (vm.heap).(a + 2) <- stack_to_heap_item v_right in
  push (STACK_HI a, vm3)

let do_fst vm =
  let v, vm1 = pop_top vm in
  match v with
  | STACK_HI a -> (
    match (vm1.heap).(a) with
    | HEAP_HEADER (_, HT_PAIR) ->
        push (heap_to_stack_item (vm.heap).(a + 1), vm1)
    | _ -> Errors.complain "do_fst : unexpectd heap item" )
  | _ -> Errors.complain "do_fst : expecting heap pointer on stack"

let do_snd vm =
  let v, vm1 = pop_top vm in
  match v with
  | STACK_HI a -> (
    match (vm1.heap).(a) with
    | HEAP_HEADER (_, HT_PAIR) ->
        push (heap_to_stack_item (vm.heap).(a + 2), vm1)
    | _ -> Errors.complain "do_snd : unexpectd heap item" )
  | _ -> Errors.complain "do_snd : expecting heap pointer on stack"

let mk_inl vm =
  let v, vm1 = pop_top vm in
  let a, vm2 = allocate (2, vm1) in
  let header = HEAP_HEADER (2, HT_INL) in
  let _ = (vm2.heap).(a) <- header in
  let _ = (vm2.heap).(a + 1) <- stack_to_heap_item v in
  push (STACK_HI a, vm2)

let mk_inr vm =
  let v, vm1 = pop_top vm in
  let a, vm2 = allocate (2, vm1) in
  let header = HEAP_HEADER (2, HT_INR) in
  let _ = (vm2.heap).(a) <- header in
  let _ = (vm2.heap).(a + 1) <- stack_to_heap_item v in
  push (STACK_HI a, vm2)

let case (i, vm) =
  let c, vm1 = pop_top vm in
  match c with
  | STACK_HI a -> (
      let vm2 = push (heap_to_stack_item (vm.heap).(a + 1), vm1) in
      match (vm1.heap).(a) with
      | HEAP_HEADER (_, HT_INR) -> goto (i, vm2)
      | HEAP_HEADER (_, HT_INL) -> advance_cp vm2
      | _ ->
          Errors.complain "case: runtime error, expecting union header in heap"
      )
  | _ ->
      Errors.complain
        "case: runtime error, expecting heap index on top of stack"

let mk_ref vm =
  let v, vm1 = pop_top vm in
  let a, vm2 = allocate (1, vm1) in
  let _ = (vm2.heap).(a) <- stack_to_heap_item v in
  push (STACK_HI a, vm2)

let deref vm =
  let v, vm1 = pop_top vm in
  match v with
  | STACK_HI a -> push (heap_to_stack_item (vm1.heap).(a), vm1)
  | _ -> Errors.complain "deref"

let assign vm =
  let c1, vm1 = pop_top vm in
  let c2, _vm2 = pop_top vm1 in
  match c2 with
  | STACK_HI a ->
      if vm.sp < vm.heap_bound then
        let _ = (vm.heap).(a) <- stack_to_heap_item c1 in
        push (STACK_UNIT, vm)
      else {vm with status= HeapIndexOutOfBound}
  | _ -> Errors.complain "assing: runtime error, expecting heap index on stack"

let test (i, vm) =
  pop
    ( 1
    , if stack_top vm = STACK_BOOL true then advance_cp vm else {vm with cp= i}
    )

let return vm =
  let current_fp = vm.fp in
  match ((vm.stack).(current_fp), (vm.stack).(vm.fp + 1)) with
  | STACK_FP saved_fp, STACK_RA k ->
      let return_value = stack_top vm in
      push (return_value, {vm with cp= k; fp= saved_fp; sp= current_fp - 2})
  | _ -> Errors.complain "return : malformed stack frame"

let fetch fp vm = function
  | STACK_LOCATION offset -> (vm.stack).(fp + offset)
  | HEAP_LOCATION offset ->
    match (vm.stack).(fp - 1) with
    | STACK_HI a -> heap_to_stack_item (vm.heap).(a + offset + 1)
    | _ -> Errors.complain "search : expecting closure pointer"

let lookup fp vm vlp = push (fetch fp vm vlp, vm)

let mk_closure = function
  | (_, Some i), n, vm ->
      let a, vm1 = allocate (2 + n, vm) in
      let header = HEAP_HEADER (2 + n, HT_CLOSURE) in
      let code_address = HEAP_CI i in
      let _ = (vm1.heap).(a) <- header in
      let _ = (vm1.heap).(a + 1) <- code_address in
      let rec aux m =
        if m = n then ()
        else
          let v = stack_to_heap_item (vm1.stack).(vm.sp - (m + 1)) in
          let _ = (vm1.heap).(a + m + 2) <- v in
          aux (m + 1)
      in
      let _ = aux 0 in
      let vm2 = pop (n, vm1) in
      push (STACK_HI a, vm2)
  | (_, None), _, _ ->
      Errors.complain "mk_closure : internal error, no address in closure!"

let apply vm =
  match stack_top vm with
  | STACK_HI a -> (
    match (vm.heap).(a + 1) with
    | HEAP_CI i ->
        let new_fp = vm.sp in
        let saved_fp = STACK_FP vm.fp in
        let return_index = STACK_RA (vm.cp + 1) in
        let new_vm = {vm with cp= i; fp= new_fp} in
        push (return_index, push (saved_fp, new_vm))
    | _ -> Errors.complain "apply: runtime error, expecting code index in heap"
    )
  | _ ->
      Errors.complain
        "apply: runtime error, expecting heap index on top of stack"

let step vm =
  match get_instruction vm with
  | UNARY op -> advance_cp (perform_unary (op, vm))
  | OPER op -> advance_cp (perform_op (op, vm))
  | MK_PAIR -> advance_cp (mk_pair vm)
  | FST -> advance_cp (do_fst vm)
  | SND -> advance_cp (do_snd vm)
  | MK_INL -> advance_cp (mk_inl vm)
  | MK_INR -> advance_cp (mk_inr vm)
  | PUSH c -> advance_cp (push (c, vm))
  | APPLY -> apply vm
  | LOOKUP vp -> advance_cp (lookup vm.fp vm vp)
  | RETURN -> return vm
  | MK_CLOSURE (l, n) -> advance_cp (mk_closure (l, n, vm))
  (* | SWAP -> advance_cp (swap vm) *)
  | POP -> advance_cp (pop (1, vm))
  | LABEL _l -> advance_cp vm
  | DEREF -> advance_cp (deref vm)
  | MK_REF -> advance_cp (mk_ref vm)
  | ASSIGN -> advance_cp (assign vm)
  | HALT -> {vm with status= Halted}
  | GOTO (_, Some i) -> goto (i, vm)
  | TEST (_, Some i) -> test (i, vm)
  | CASE (_, Some i) -> case (i, vm)
  | _ -> Errors.complain ("step : bad state = " ^ string_of_state vm ^ "\n")

let rec driver (options: Options.t) n vm =
  let _ =
    if options.verbose_back then
      print_string
        ( "========== state " ^ string_of_int n ^ " ==========\n"
        ^ string_of_state vm ^ "\n" )
    else ()
  in
  if vm.status = Running then driver options (n + 1) (step vm) else vm

let map_instruction_labels f = function
  | GOTO (lab, _) -> GOTO (lab, Some (f lab))
  | TEST (lab, _) -> TEST (lab, Some (f lab))
  | CASE (lab, _) -> CASE (lab, Some (f lab))
  | MK_CLOSURE ((lab, _), n) -> MK_CLOSURE ((lab, Some (f lab)), n)
  | inst -> inst

let find l y = List.assoc y l

(* put code listing into an array, associate an array index to each label *)
let load instr_list =
  (* find array index for each label *)
  let mk_label_to_address l =
    let rec aux carry k = function
      | [] -> carry
      | LABEL lab :: rest -> aux ((lab, k) :: carry) (k + 1) rest
      | _ :: rest -> aux carry (k + 1) rest
    in
    aux [] 0 l
  in
  let label_to_address = mk_label_to_address instr_list in
  let locate_instr = map_instruction_labels (find label_to_address) in
  let located_instr_list = List.map locate_instr instr_list in
  let result = Array.of_list located_instr_list in
  (result, Array.length result)

let initial_state (options: Options.t) l =
  let code_array, c_bound = load l in
  let _ =
    if options.verbose_back then
      print_string
        ( "\nInstalled Code = \n"
        ^ string_of_installed_code (code_array, c_bound) )
    else ()
  in
  { stack_bound= options.stack_max
  ; heap_bound= options.heap_max
  ; code_bound= c_bound
  ; stack= Array.make options.stack_max (STACK_INT 0)
  ; heap= Array.make options.heap_max (HEAP_INT 0)
  ; code= code_array
  ; sp= 0
  ; fp= 0
  ; cp= 0
  ; hp= 0
  ; status= Running }

let first_frame vm =
  let saved_fp = STACK_FP 0 in
  let return_index = STACK_RA 0 in
  push (return_index, push (saved_fp, vm))

let run (options: Options.t) l =
  let vm = driver options 1 (first_frame (initial_state options l)) in
  match vm.status with
  | Halted -> vm
  | status ->
      Errors.complain ("run : stopped wth status " ^ string_of_status status)


let interpret options instructions = run options instructions

