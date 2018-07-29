
let%expect_test "let1" =
  Test.run {|
    let x : int = 1
    in (
      let x : int = 88 in 
        x + 12 
      end
    ) + x 
    end 
  |};

  [%expect {|
    101
    101
    101
    101
    (Int 101) |}]

let%expect_test "let2" =
  Test.run {|
    let x : int = 100
    in let y : int = 1 
      in begin
          x + y
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
