
let%expect_test "ref1" =
  Test.run "let x : int ref = (ref 50) in !x + !x + 1 end";

  [%expect {|
    101
    101
    101
    101
    (Int 101) |}]

let%expect_test "ref2" =
  Test.run {|
    let x : (int ref) ref = ref (ref 777)  
    in begin 
      x := (ref 101); 
      !!x 
    end 
    end 
  |};

  [%expect {|
    101
    101
    101
    101
    (Int 101) |}]

let%expect_test "ref3" =
  Test.run {|
    let x : (int ref) ref = ref (ref 777)  
    in begin 
        (!x) := 101; 
        !!x 
       end 
    end 
  |};

  [%expect {|
    101
    101
    101
    101
    (Int 101) |}]
