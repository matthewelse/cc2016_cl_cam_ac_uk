
let%expect_test "lt1" =
  Test.run "31 < 1889";

  [%expect {|
    true
    true
    true
    true
    (Bool true) |}]

let%expect_test "lt2" =
  Test.run "1889 < 31";

  [%expect {|
    false
    false
    false
    false
    (Bool false) |}]
