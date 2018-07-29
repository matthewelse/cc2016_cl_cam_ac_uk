let%expect_test "eq1" =
  Test.run "101 = 101";

  [%expect {|
    true
    true
    true
    true
    (Bool true) |}]

let%expect_test "eq2" =
  Test.run "true = true";

  [%expect {|
    true
    true
    true
    true
    (Bool true) |}]

let%expect_test "eq3" =
  Test.run "101 = 102";

  [%expect {|
    false
    false
    false
    false
    (Bool false) |}]

let%expect_test "eq4" =
  Test.run "true = false";

  [%expect {|
    false
    false
    false
    false
    (Bool false) |}]
