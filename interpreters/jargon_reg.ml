open Core
open Common
open Common.Jargon_reg

let register_to_heap_item = function
  | Register_item.Int i -> Heap_item.Int i
  | Bool b -> Bool b
  | Unit -> Unit
  | Heap_index i -> Heap_index i
  | Return_address i -> Code_pointer i
  | Frame_pointer _i ->
      Errors.complain "register_to_heap_item: no frame pointer allowed on heap"

let heap_to_register_item = function
  | Heap_item.Int i -> Register_item.Int i
  | Bool b -> Bool b
  | Unit -> Unit
  | Heap_index i -> Heap_index i
  | Code_pointer i -> Return_address i
  | Header (_, _) ->
      Errors.complain "heap_to_register_item: heap header not allowed on stack"

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

let do_unary oper value =
  match (oper, value) with
  | Oper.NOT, Register_item.Bool m -> Register_item.Bool (not m)
  | NEG, Int m -> Int (-m)
  | READ, Unit -> Int (Utils.read_int ())
  | op, _ ->
      Errors.complain
        ("do_unary: malformed unary operator: " ^ Oper.string_of_uop op)

let do_oper oper left right =
  match (oper, left, right) with
  | Oper.AND, Register_item.Bool m, Register_item.Bool n ->
      Register_item.Bool (m && n)
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
    ; stack: Register_item.t array
    ; register_file: Register_item.t Register.Map.t
    ; heap: Heap_item.t array
    ; code: Instruction.t array
    ; mutable sp: stack_index
    ; (* stack pointer *)
      mutable cp: code_index
    ; (* code pointer  *)
      mutable hp: heap_index
    ; (* next free     *)
      mutable status: Status.t }
  [@@deriving sexp_of]

  let set_register t (reg: Register.t) (value: Register_item.t) =
    {t with register_file= Map.set t.register_file ~key:reg ~data:value}

  let get_register t reg =
    match Register.Map.find t.register_file reg with
    | None -> Register_item.Int 0
    | Some x -> x

  let get_fp t =
    match get_register t Frame_pointer with
    | Frame_pointer fp -> fp
    | _ ->
        Errors.complain
          "Frame pointer register contains a value that is not a frame pointer."

  let _get_ra t =
    match get_register t Return_address with
    | Return_address ra -> ra
    | _ ->
        Errors.complain
          "Return address register contains a value that is not a return \
           address."

  let get_instruction t = (t.code).(t.cp)

  let to_string t =
    let instr = get_instruction t in
    let cp = sprintf !"cp = %d -> %{Instruction}" t.cp instr in
    let hp = sprintf !"hp = %d" t.hp in
    let stack = sprintf "Stack =\n%s" (string_of_stack t.stack t.sp) in
    let heap =
      if t.hp = 0 then "Heap empty"
      else sprintf "Heap =\n%s" (string_of_heap t.heap t.hp)
    in
    let registers = string_of_register_file t.register_file in
    String.concat ~sep:"\n" [cp; hp; stack; heap; registers]

  let stack_top t = (t.stack).(t.sp - 1)

  let rec string_of_heap_item t x =
    match (t.heap).(x) with
    | Heap_index x -> string_of_heap_item t x
    | Code_pointer x -> Instruction.to_string (t.code).(x)
    | Header (_, typ) -> (
      match typ with
      | Pair ->
          sprintf "(%s, %s)"
            (string_of_heap_item t (x + 1))
            (string_of_heap_item t (x + 2))
      | Inl -> sprintf "(inl %s)" (string_of_heap_item t (x + 1))
      | Inr -> sprintf "(inr %s)" (string_of_heap_item t (x + 1))
      | Closure -> sprintf "(closure %s)" (string_of_heap_item t (x + 1)) )
    | _ as item -> Heap_item.to_string item

  let string_of_value t =
    match get_register t Return_value with
    | Heap_index x -> string_of_heap_item t x
    | _ as r -> Register_item.to_string r

  (* cp := cp + 1  *)
  let advance_cp t =
    if t.cp < t.code_bound then {t with cp= t.cp + 1}
    else {t with status= CodeIndexOutOfBound}

  let goto t i = {t with cp= i}

  (* this just discards values off the top of the heap. *)
  let pop t n =
    if 0 <= t.sp - n then {t with sp= t.sp - n}
    else {t with status= StackUnderflow}

  let _pop_top t reg =
    let c = stack_top t in
    set_register t reg c

  (* pop value of reg onto stack  *)
  let push t reg =
    let c = get_register t reg in
    if t.sp < t.stack_bound then
      let _ = (t.stack).(t.sp) <- c in
      {t with sp= t.sp + 1}
    else {t with status= StackIndexOutOfBound}

  (* reg1 := reg2 op reg3 *)
  let perform_op t op dest left right =
    let v_right = get_register t right in
    let v_left = get_register t left in
    let result = do_oper op v_left v_right in
    set_register t dest result

  (* reg1 := op reg2 *)
  let perform_unary t op reg1 reg2 =
    let v = get_register t reg2 in
    let result = do_unary op v in
    set_register t reg1 result

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

  (* reg1 := (reg2, reg3) *)
  let mk_pair t reg1 reg2 reg3 =
    let v_left = get_register t reg2 in
    let v_right = get_register t reg3 in
    let a, t = allocate_exn t 3 in
    let header = Heap_item.Header (3, Pair) in
    (t.heap).(a) <- header ;
    (t.heap).(a + 1) <- register_to_heap_item v_left ;
    (t.heap).(a + 2) <- register_to_heap_item v_right ;
    (* reg1 := pointer to the pair *)
    set_register t reg1 (Heap_index a)

  (* reg1 := fst reg2 *)
  let do_fst t reg1 reg2 =
    match get_register t reg2 with
    | Heap_index a -> (
      match (t.heap).(a) with
      | Header (_, Pair) ->
          set_register t reg1 (heap_to_register_item (t.heap).(a + 1))
      | _ -> Errors.complain "do_fst : unexpected heap item" )
    | _ -> Errors.complain "do_fst : expecting heap pointer on stack"

  (* reg1 := snd reg2 *)
  let do_snd t reg1 reg2 =
    match get_register t reg2 with
    | Heap_index a -> (
      match (t.heap).(a) with
      | Header (_, Pair) ->
          set_register t reg1 (heap_to_register_item (t.heap).(a + 2))
      | _ -> Errors.complain "do_snd : unexpected heap item" )
    | _ -> Errors.complain "do_snd : expecting heap pointer on stack"

  (* reg1 := mk_inl reg2 *)
  let mk_inl t reg1 reg2 =
    let v = get_register t reg2 in
    let a, t = allocate_exn t 2 in
    let header = Heap_item.Header (2, Inl) in
    (t.heap).(a) <- header ;
    (t.heap).(a + 1) <- register_to_heap_item v ;
    set_register t reg1 (Heap_index a)

  (* reg1 := mk_inr reg2 *)
  let mk_inr t reg1 reg2 =
    let v = get_register t reg2 in
    let a, t = allocate_exn t 2 in
    let header = Heap_item.Header (2, Inr) in
    (t.heap).(a) <- header ;
    (t.heap).(a + 1) <- register_to_heap_item v ;
    set_register t reg1 (Heap_index a)

  (* match reg2 with
   * | Inr x -> (goto i)
   * | Inl x -> (advance_cp) *)
  let case t reg1 reg2 i =
    let c = get_register t reg2 in
    match c with
    | Heap_index a -> (
        let t = set_register t reg1 (heap_to_register_item (t.heap).(a + 1)) in
        match (t.heap).(a) with
        | Header (_, Inr) -> goto t i
        | Header (_, Inl) -> advance_cp t
        | _ ->
            Errors.complain
              "case: runtime error, expecting union header in heap" )
    | _ ->
        Errors.complain
          "case: runtime error, expecting heap index on top of stack"

  (* reg1 := ref reg2 *)
  let mk_ref t reg1 reg2 =
    let v = get_register t reg2 in
    let a, t = allocate_exn t 1 in
    (t.heap).(a) <- register_to_heap_item v ;
    set_register t reg1 (Heap_index a)

  (* reg1 := *reg2 *)
  let deref t reg1 reg2 =
    let v = get_register t reg2 in
    match v with
    | Heap_index a -> set_register t reg1 (heap_to_register_item (t.heap).(a))
    | _ -> Errors.complain "deref"

  (* *reg1 := reg2 *)
  let assign t reg1 reg2 =
    let c1 = get_register t reg1 in
    let c2 = get_register t reg2 in
    match c1 with
    | Heap_index a ->
        if a < t.heap_bound then (
          (t.heap).(a) <- register_to_heap_item c2 ;
          t )
        else {t with status= HeapIndexOutOfBound}
    | _ ->
        Errors.complain "assign: runtime error, expecting heap index on stack"

  (* if reg then pc += 1 else pc := i *)
  let test t i reg =
    match get_register t reg with
    | Register_item.Bool true -> advance_cp t
    | _ -> {t with cp= i}

  (* return reg*)
  let return t reg =
    let fp = get_fp t in
    match ((t.stack).(fp), (t.stack).(fp + 1)) with
    | Frame_pointer saved_fp, Return_address k ->
        let return_value = get_register t reg in
        let t = set_register t Return_value return_value in
        let t = set_register t Frame_pointer (Frame_pointer saved_fp) in
        {t with cp= k; sp= fp - 2}
    | _ -> Errors.complain "return : malformed stack frame"

  let fetch t fp = function
    | STACK_LOCATION offset -> (t.stack).(fp + offset)
    | HEAP_LOCATION offset ->
      match (t.stack).(fp - 1) with
      | Heap_index a -> heap_to_register_item (t.heap).(a + offset + 1)
      | _ -> Errors.complain "search : expecting closure pointer"

  (* reg := lookup(fp, vlp) *)
  let lookup t fp vlp reg = set_register t reg (fetch t fp vlp)

  (* take the n top items off the stack, and bundle them into a closure. *)
  (* reg := mk_closure(n, i?) *)
  let mk_closure t n reg = function
    | _, Some i ->
        let a, t = allocate_exn t (2 + n) in
        let header = Heap_item.Header (2 + n, Closure) in
        let code_address = Heap_item.Code_pointer i in
        (t.heap).(a) <- header ;
        (t.heap).(a + 1) <- code_address ;
        let rec aux m =
          if m <> n then (
            let v = register_to_heap_item (t.stack).(t.sp - (m + 1)) in
            (t.heap).(a + m + 2) <- v ;
            aux (m + 1) )
        in
        aux 0 ;
        let t = pop t n in
        set_register t reg (Heap_index a)
    | _, None ->
        Errors.complain "mk_closure : internal error, no address in closure!"

  (* run a closure *)
  let apply t reg =
    match get_register t reg with
    | Heap_index a -> (
      match (t.heap).(a + 1) with
      | Code_pointer i ->
          (* save the frame pointer *)
          let t = push t Frame_pointer in
          (* save the return address *)
          let t = push t Return_address in
          let new_fp = t.sp in
          let new_ra = t.cp + 1 in
          let t = set_register t Frame_pointer (Frame_pointer new_fp) in
          let t = set_register t Return_address (Return_address new_ra) in
          {t with cp= i}
      | _ ->
          Errors.complain "apply: runtime error, expecting code index in heap"
      )
    | _ ->
        Errors.complain
          "apply: runtime error, expecting heap index on top of stack"

  let mov_register t dest src =
    let value = get_register t src in
    set_register t dest value

  let step t =
    match get_instruction t with
    | Set (dest, value) -> advance_cp (set_register t dest value)
    | Mov (dest, src) -> advance_cp (mov_register t dest src)
    | Unary (op, dest, reg) -> advance_cp (perform_unary t op dest reg)
    (* TODO me390: we could unify Oper and Make_pair *)
    | Oper (op, dest, (op1, op2)) ->
        advance_cp (perform_op t op dest op1 op2)
    | Make_pair (dest, (op1, op2)) -> advance_cp (mk_pair t dest op1 op2)
    | Fst (dest, op) -> advance_cp (do_fst t dest op)
    | Snd (dest, op) -> advance_cp (do_snd t dest op)
    | Make_inl (dest, op) -> advance_cp (mk_inl t dest op)
    | Make_inr (dest, op) -> advance_cp (mk_inr t dest op)
    | Push reg -> advance_cp (push t reg)
    | Apply reg -> apply t reg
    | Lookup (reg, vp) -> advance_cp (lookup t (get_fp t) vp reg)
    | Return reg -> return t reg
    | Make_closure (reg, l, n) -> advance_cp (mk_closure t n reg l)
    | Label _l -> advance_cp t
    | Deref (dest, ptr) -> advance_cp (deref t dest ptr)
    | Make_ref (dest, src) -> advance_cp (mk_ref t dest src)
    | Assign (ptr, src) -> advance_cp (assign t ptr src)
    | Halt -> {t with status= Halted}
    | Goto (_, Some i) -> goto t i
    | Test (reg, (_, Some i)) -> test t i reg
    | Case (reg1, reg2, (_, Some i)) -> case t reg1 reg2 i
    | _ -> Errors.complain ("step : bad state = " ^ to_string t ^ "\n")

  let rec driver t ~(options: Options.t) ~n =
    if options.verbose_vm then
      print_string
        ( "========== state " ^ string_of_int n ^ " ==========\n" ^ to_string t
        ^ "\n" ) ;
    if t.status = Running then driver (step t) ~options ~n:(n + 1) else t

  let map_instruction_labels f = function
    | Instruction.Goto (lab, _) -> Instruction.Goto (lab, Some (f lab))
    | Test (reg, (lab, _)) -> Test (reg, (lab, Some (f lab)))
    | Case (reg1, reg2, (lab, _)) -> Case (reg1, reg2, (lab, Some (f lab)))
    | Make_closure (reg, (lab, _), n) ->
        Make_closure (reg, (lab, Some (f lab)), n)
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
    let saved_fp = Register_item.Frame_pointer 0 in
    let return_index = Register_item.Return_address 0 in
    let t = set_register t Frame_pointer saved_fp in
    let t = set_register t Return_address return_index in
    let t = push t Frame_pointer in
    push t Return_address

  let create (options: Options.t) code =
    let code_array, c_bound = load code in
    let _ =
      if options.verbose_vm then
        print_string
          ( "\nInstalled Code = \n"
          ^ string_of_installed_code (code_array, c_bound) )
      else ()
    in
    { stack_bound= options.stack_max
    ; heap_bound= options.heap_max
    ; code_bound= c_bound
    ; stack= Array.init options.stack_max ~f:(fun _ -> Register_item.Int 0)
    ; heap= Array.init options.heap_max ~f:(fun _ -> Heap_item.Int 0)
    ; register_file= Register.Map.empty
    ; code= code_array
    ; sp= 0
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
