(* This test shows why we need closures on the heap *)

let%expect_test "fun" =
  Test.run {|
    let f(y : int) : int -> int =
      let g(x :int) : int = y + x in g end
    in
      let add21 : int -> int  = f(21) in 
          let add17 : int -> int  = f(17) in 
             add17(3) + add21(60)
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
