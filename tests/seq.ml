
let%expect_test "seq1" =
  Test.run {|
    let x : int ref = ref 100 in
      let y : int ref = ref 1 in
        begin !x + !y end   
      end 
    end 
  |};

  [%expect {|
    101
    101
    101
    101
    (Int 101) |}]

let%expect_test "seq2" =
  Test.run {|
    let x : int ref = ref 0 in
      let y : int ref = ref 0 in
        begin 
          x := 10; 
          y := 1; 
          x := 100; 
          !x + !y
        end   
      end 
    end 
  |};

  [%expect {|
    101
    101
    101
    101
    (Int 101) |}]

let%expect_test "seq3" =
  Test.run {|
    let x : (int -> int) ref = ref (fun (z : int) -> z + 17 end) in
      let y : int ref = ref 0 in
        begin 
          y := !x 4; 
          x := (fun (w : int) -> w * 10 end); 
          ((!x) 8) + !y
        end 
      end 
    end 
  |};

  [%expect {|
    101
    101
    101
    101
    (Int 101) |}]
