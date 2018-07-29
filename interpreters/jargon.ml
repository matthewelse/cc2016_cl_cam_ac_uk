open Core
open Common
open Common.Jargon

let stack_to_heap_item = function
  | Stack_item.Int i -> Heap_item.Int i
  | Bool b -> Bool b
  | Unit -> Unit
  | Heap_index i -> Heap_index i
  | Return_address i -> Code_pointer i
  | Frame_pointer _i ->
      Errors.complain "stack_to_heap_item: no frame pointer allowed on heap"

let heap_to_stack_item = function
  | Heap_item.Int i -> Stack_item.Int i
  | Bool b -> Bool b
  | Unit -> Unit
  | Heap_index i -> Heap_index i
  | Code_pointer i -> Return_address i
  | Header (_, _) ->
      Errors.complain "heap_to_stack_item : heap header not allowed on stack"

module Status = struct
  type t =
    | Halted
    | Running
    | CodeIndexOutOfBound
    | StackIndexOutOfBound
    | HeapIndexOutOfBound
    | StackUnderflow
  [@@deriving sexp_of]

  let to_string = function
    | Halted -> "halted"
    | Running -> "running"
    | CodeIndexOutOfBound -> "code index out-of-bound"
    | StackIndexOutOfBound -> "stack index out-of-bound"
    | HeapIndexOutOfBound -> "heap index out-of-bound"
    | StackUnderflow -> "stack underflow"
end

let do_unary = function
  | Oper.NOT, Stack_item.Bool m -> Stack_item.Bool (not m)
  | NEG, Int m -> Int (-m)
  | READ, Unit -> Int (Utils.read_int ())
  | op, _ ->
      Errors.complain
        ("do_unary: malformed unary operator: " ^ Oper.string_of_uop op)

let do_oper = function
  | Oper.AND, Stack_item.Bool m, Stack_item.Bool n -> Stack_item.Bool (m && n)
  | OR, Bool m, Bool n -> Bool (m || n)
  | EQB, Bool m, Bool n -> Bool (m = n)
  | LT, Int m, Int n -> Bool (m < n)
  | EQI, Int m, Int n -> Bool (m = n)
  | ADD, Int m, Int n -> Int (m + n)
  | SUB, Int m, Int n -> Int (m - n)
  | MUL, Int m, Int n -> Int (m * n)
  | DIV, Int m, Int n -> Int (m / n)
  | op, _, _ ->
      Errors.complain
        ("do_oper: malformed binary operator: " ^ Oper.string_of_bop op)

let find l y = List.Assoc.find_exn l y ~equal:( = )

module Vm_state = struct
  type t =
    { stack_bound: stack_index
    ; code_bound: code_index
    ; heap_bound: code_index
    ; stack: Stack_item.t array
    ; heap: Heap_item.t array
    ; code: Instruction.t array
    ; mutable sp: stack_index
    ; (* stack pointer *)
      mutable fp: stack_index
    ; (* frame pointer *)
      mutable cp: code_index
    ; (* code pointer  *)
      mutable hp: heap_index
    ; (* next free     *)
      mutable status: Status.t }
  [@@deriving sexp_of]

  let get_instruction t = (t.code).(t.cp)

  let to_string t =
    let instr = get_instruction t in
    let cp = sprintf !"cp = %d -> %{Instruction}" t.cp instr in
    let fp = sprintf "fp = %d" t.fp in
    let stack = sprintf "Stack = %s" (string_of_stack (t.sp, t.stack)) in
    let heap = if t.hp = 0 then "" else string_of_heap t.heap t.hp in
    String.concat ~sep:"\n" [cp; fp; stack; heap]

  let stack_top t = (t.stack).(t.sp - 1)

  let string_of_value t = Stack_item.to_string (stack_top t)

  (* cp := cp + 1  *)
  let advance_cp t =
    if t.cp < t.code_bound then {t with cp= t.cp + 1}
    else {t with status= CodeIndexOutOfBound}

  let goto t i = {t with cp= i}

  let pop t n =
    if 0 <= t.sp - n then {t with sp= t.sp - n}
    else {t with status= StackUnderflow}

  let pop_top t =
    let c = stack_top t in
    (c, pop t 1)

  (* pop c onto stack  *)
  let push t c =
    if t.sp < t.stack_bound then
      let _ = (t.stack).(t.sp) <- c in
      {t with sp= t.sp + 1}
    else {t with status= StackIndexOutOfBound}

  let perform_op t op =
    let v_right, t' = pop_top t in
    let v_left, t'' = pop_top t' in
    push t'' (do_oper (op, v_left, v_right))

  let perform_unary t op =
    let v, t' = pop_top t in
    push t' (do_unary (op, v))

  (* implement garbage collection!

     This should free up all heap space
     not reachable from the stack.

     Might also increase heap size.

     Result:
     None = no progress
     Some(vm') = progress made, resulting in vm'
  *)
  let invoke_garbage_collection _t = None

  let allocate_exn t n =
    let hp = t.hp in
    if hp + n < t.heap_bound then (hp, {t with hp= hp + n})
    else
      match invoke_garbage_collection t with
      | None -> Errors.complain "allocate : heap exhausted"
      | Some t' ->
          if t'.hp + n < t'.heap_bound then (t'.hp, {t' with hp= t'.hp + n})
          else Errors.complain "allocate : heap exhausted"

  let mk_pair t =
    let v_right, t' = pop_top t in
    let v_left, t'' = pop_top t' in
    let a, t''' = allocate_exn t'' 3 in
    let header = Heap_item.Header (3, Pair) in
    let _ = (t.heap).(a) <- header in
    let _ = (t.heap).(a + 1) <- stack_to_heap_item v_left in
    let _ = (t.heap).(a + 2) <- stack_to_heap_item v_right in
    push t''' (Heap_index a)

  let do_fst t =
    let v, t' = pop_top t in
    match v with
    | Heap_index a -> (
      match (t'.heap).(a) with
      | Header (_, Pair) -> push t' (heap_to_stack_item (t.heap).(a + 1))
      | _ -> Errors.complain "do_fst : unexpected heap item" )
    | _ -> Errors.complain "do_fst : expecting heap pointer on stack"

  let do_snd t =
    let v, t' = pop_top t in
    match v with
    | Heap_index a -> (
      match (t'.heap).(a) with
      | Header (_, Pair) -> push t' (heap_to_stack_item (t.heap).(a + 2))
      | _ -> Errors.complain "do_snd : unexpected heap item" )
    | _ -> Errors.complain "do_snd : expecting heap pointer on stack"

  let mk_inl t =
    let v, t' = pop_top t in
    let a, t'' = allocate_exn t' 2 in
    let header = Heap_item.Header (2, Inl) in
    let _ = (t''.heap).(a) <- header in
    let _ = (t''.heap).(a + 1) <- stack_to_heap_item v in
    push t'' (Heap_index a)

  let mk_inr t =
    let v, t' = pop_top t in
    let a, t'' = allocate_exn t' 2 in
    let header = Heap_item.Header (2, Inr) in
    let _ = (t''.heap).(a) <- header in
    let _ = (t''.heap).(a + 1) <- stack_to_heap_item v in
    push t'' (Heap_index a)

  let case t i =
    let c, t' = pop_top t in
    match c with
    | Heap_index a -> (
        let t'' = push t' (heap_to_stack_item (t.heap).(a + 1)) in
        match (t'.heap).(a) with
        | Header (_, Inr) -> goto t'' i
        | Header (_, Inl) -> advance_cp t''
        | _ ->
            Errors.complain
              "case: runtime error, expecting union header in heap" )
    | _ ->
        Errors.complain
          "case: runtime error, expecting heap index on top of stack"

  let mk_ref t =
    let v, t' = pop_top t in
    let a, t'' = allocate_exn t' 1 in
    (t''.heap).(a) <- stack_to_heap_item v ;
    push t'' (Heap_index a)

  let deref t =
    let v, t' = pop_top t in
    match v with
    | Heap_index a -> push t' (heap_to_stack_item (t'.heap).(a))
    | _ -> Errors.complain "deref"

  let assign t =
    let c1, t' = pop_top t in
    let c2, _t'' = pop_top t' in
    match c2 with
    | Heap_index a ->
        if t.sp < t.heap_bound then
          let _ = (t.heap).(a) <- stack_to_heap_item c1 in
          push t Unit
        else {t with status= HeapIndexOutOfBound}
    | _ ->
        Errors.complain "assign: runtime error, expecting heap index on stack"

  let test t i =
    let t' =
      if stack_top t = Stack_item.Bool true then advance_cp t
      else {t with cp= i}
    in
    pop t' 1

  let return t =
    let current_fp = t.fp in
    match ((t.stack).(current_fp), (t.stack).(t.fp + 1)) with
    | Frame_pointer saved_fp, Return_address k ->
        let return_value = stack_top t in
        push {t with cp= k; fp= saved_fp; sp= current_fp - 2} return_value
    | _ -> Errors.complain "return : malformed stack frame"

  let fetch t fp = function
    | STACK_LOCATION offset -> (t.stack).(fp + offset)
    | HEAP_LOCATION offset ->
      match (t.stack).(fp - 1) with
      | Heap_index a -> heap_to_stack_item (t.heap).(a + offset + 1)
      | _ -> Errors.complain "search : expecting closure pointer"

  let lookup t fp vlp = push t (fetch t fp vlp)

  let mk_closure t n = function
    | _, Some i ->
        let a, t' = allocate_exn t (2 + n) in
        let header = Heap_item.Header (2 + n, Closure) in
        let code_address = Heap_item.Code_pointer i in
        let _ = (t'.heap).(a) <- header in
        let _ = (t'.heap).(a + 1) <- code_address in
        let rec aux m =
          if m <> n then (
            let v = stack_to_heap_item (t'.stack).(t.sp - (m + 1)) in
            (t'.heap).(a + m + 2) <- v ;
            aux (m + 1) )
        in
        aux 0 ;
        let t'' = pop t' n in
        push t'' (Heap_index a)
    | _, None ->
        Errors.complain "mk_closure : internal error, no address in closure!"

  let apply t =
    match stack_top t with
    | Heap_index a -> (
      match (t.heap).(a + 1) with
      | Code_pointer i ->
          let new_fp = t.sp in
          let saved_fp = Stack_item.Frame_pointer t.fp in
          let return_index = Stack_item.Return_address (t.cp + 1) in
          let t' = {t with cp= i; fp= new_fp} in
          push (push t' saved_fp) return_index
      | _ ->
          Errors.complain "apply: runtime error, expecting code index in heap"
      )
    | _ ->
        Errors.complain
          "apply: runtime error, expecting heap index on top of stack"

  let step t =
    match get_instruction t with
    | Unary op -> advance_cp (perform_unary t op)
    | Oper op -> advance_cp (perform_op t op)
    | Make_pair -> advance_cp (mk_pair t)
    | Fst -> advance_cp (do_fst t)
    | Snd -> advance_cp (do_snd t)
    | Make_inl -> advance_cp (mk_inl t)
    | Make_inr -> advance_cp (mk_inr t)
    | Push c -> advance_cp (push t c)
    | Apply -> apply t
    | Lookup vp -> advance_cp (lookup t t.fp vp)
    | Return -> return t
    | Make_closure (l, n) -> advance_cp (mk_closure t n l)
    (* | SWAP -> advance_cp (swap vm) *)
    | Pop -> advance_cp (pop t 1)
    | Label _l -> advance_cp t
    | Deref -> advance_cp (deref t)
    | Make_ref -> advance_cp (mk_ref t)
    | Assign -> advance_cp (assign t)
    | Halt -> {t with status= Halted}
    | Goto (_, Some i) -> goto t i
    | Test (_, Some i) -> test t i
    | Case (_, Some i) -> case t i
    | _ -> Errors.complain ("step : bad state = " ^ to_string t ^ "\n")

  let rec driver t ~(options: Options.t) ~n =
    if options.verbose_back then
      print_string
        ( "========== state " ^ string_of_int n ^ " ==========\n" ^ to_string t
        ^ "\n" ) ;
    if t.status = Running then driver (step t) ~options ~n:(n + 1) else t

  let map_instruction_labels f = function
    | Instruction.Goto (lab, _) -> Instruction.Goto (lab, Some (f lab))
    | Test (lab, _) -> Test (lab, Some (f lab))
    | Case (lab, _) -> Case (lab, Some (f lab))
    | Make_closure ((lab, _), n) -> Make_closure ((lab, Some (f lab)), n)
    | inst -> inst

  (* put code listing into an array, associate an array index to each label *)
  let load instr_list =
    (* find array index for each label *)
    let mk_label_to_address l =
      let rec aux carry k = function
        | [] -> carry
        | Instruction.Label lab :: rest -> aux ((lab, k) :: carry) (k + 1) rest
        | _ :: rest -> aux carry (k + 1) rest
      in
      aux [] 0 l
    in
    let label_to_address = mk_label_to_address instr_list in
    let locate_instr = map_instruction_labels (find label_to_address) in
    let located_instr_list = List.map ~f:locate_instr instr_list in
    let result = Array.of_list located_instr_list in
    (result, Array.length result)

  let first_frame t =
    let saved_fp = Stack_item.Frame_pointer 0 in
    let return_index = Stack_item.Return_address 0 in
    push (push t saved_fp) return_index

  let create (options: Options.t) code =
    let code_array, c_bound = load code in
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
    ; stack= Array.init options.stack_max ~f:(fun _ -> Stack_item.Int 0)
    ; heap= Array.init options.heap_max ~f:(fun _ -> Heap_item.Int 0)
    ; code= code_array
    ; sp= 0
    ; fp= 0
    ; cp= 0
    ; hp= 0
    ; status= Running }
end

let run (options: Options.t) l =
  let vm =
    Vm_state.create options l |> Vm_state.first_frame
    |> Vm_state.driver ~options ~n:1
  in
  match vm.status with
  | Halted -> vm
  | status ->
      Errors.complain ("run : stopped wth status " ^ Status.to_string status)

let interpret options instructions = run options instructions
