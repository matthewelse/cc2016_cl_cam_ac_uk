
let%expect_test "nest1" =
  Test.run {|
    let x1 : int = 1 
    in let x2 : int = 2 + x1 
      in let x3 : int = 3 + x1 + x2 
        in x3 
        end 
      end 
    end 
  |};

  [%expect {|
    7
    7
    7
    7
    (Int 7) |}]

let%expect_test "nest2" =
  Test.run {|
    let x1 : int = 1 
    in let x2 : int = 2 + x1 
      in let x3 : int = 3 + x1 + x2 
        in let g(y : int) : int = y + x3 
          in let h(z : int) : int = z + g(z) 
            in g(h(g(x3)))
            end 
          end 
        end 
      end 
    end 
  |};

  [%expect {|
    42
    42
    42
    42
    (Int 42) |}]
