
let%expect_test "sub" =
  Test.run "102 - 1";

  [%expect {|
    101
    101
    101
    101
    101 |}]
