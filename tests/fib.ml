open Core

let fib n =
  sprintf {|
    let fib( m : int) : int = 
      if m = 0
      then 1 
      else
        if m = 1 
        then 1 
        else fib (m - 1) + fib (m -2) 
        end 
      end 
    in 
      fib(%d) 
    end 
  |} n |> Test.run

let fibv2 (n, m) =
  sprintf {| 
    let h(p : int * int) : int  = 
      let fib( m : int) : int = 
        if m = 0
        then snd p  
        else if m = 1
             then snd p 
             else fib (m - 1) + fib (m -2) 
             end 
        end 
       in 
          fib(fst p) 
       end 
    in 
        h(%d, %d) 
    end 
  |} n m |> Test.run

let%expect_test "fib" =
  List.iter [0; 1; 2; 3; 4; 5; 10] ~f:fib;

  [%expect {|
    1
    1
    1
    1
    (Int 1)
    1
    1
    1
    1
    (Int 1)
    2
    2
    2
    2
    (Int 2)
    3
    3
    3
    3
    (Int 3)
    5
    5
    5
    5
    (Int 5)
    8
    8
    8
    8
    (Int 8)
    89
    89
    89
    89
    (Int 89) |}]

let%expect_test "fibv2" =
  List.iter [5, 1] ~f:fibv2;

  [%expect {|
    8
    8
    8
    8
    (Int 8)
  |}]
