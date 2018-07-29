let%expect_test "bool" =
  Test.run "true && (~(true || false))";
  
  [%expect {|
    false
    false
    false
    false
    (Bool false) |}]
