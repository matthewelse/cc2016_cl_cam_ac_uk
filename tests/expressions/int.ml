let%expect_test "int" =
  Test.run "101"; 

  [%expect {|
    101
    101
    101
    101
    (Int 101) |}]
