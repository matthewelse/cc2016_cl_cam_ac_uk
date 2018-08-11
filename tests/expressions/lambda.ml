
let%expect_test "lambda1" =
  Test.run "(fun (x : int) -> x end) (99 + 2)";

  [%expect {|
    101
    101
    101
    101
    (Int 101) |}]

let%expect_test "lambda2" =
  Test.run {|
    (fun (d : int + int) ->
        case d of
        inl (x : int) -> x + 2 
      | inr (y : int) -> 17
        end
      end)
    (inl int 99) 
  |};

  [%expect {|
    101
    101
    101
    101
    (Int 101) |}]

let%expect_test "lambda3" =
  Test.run {|
    (fun (d : int + int) ->
      case d of
      inl (x : int) -> x + 2
    | inr (y : int) -> 17 
      end
    end) (inl int 99) 
  |};

  [%expect {|
    101
    101
    101
    101
    (Int 101) |}]
