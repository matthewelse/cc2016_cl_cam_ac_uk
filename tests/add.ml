
let%expect_test "add" =
  Test.run "1 + 100";

  [%expect {|
    101
    101
    101
    101
    101 |}]

let%expect_test "add2" =
  Test.run "1 + (88 + 12)";

  [%expect {|
    101
    101
    101
    101
    101 |}]


let%expect_test "add3" =
  Test.run {|
    let add(p : int * int) : int = (fst p) + (snd p)
    in
      add (1, add (88, 12))
    end
  |};

  [%expect {|
    101
    101
    101
    101
    101 |}]

let%expect_test "add4" =
  Test.run {|
    let add(p : int * int) : int =
        let h(z : int) : int = z + (snd p)
        in
          h(fst p)
        end
    in
      add(1, add(88, 12))
    end
  |};

  [%expect {|
    101
    101
    101
    101
    101 |}]

let%expect_test "add5" =
  Test.run {|
    let add(p : int * int) : int =
        let g(w : int) : int = w
        in
          let h(z : int) : int = z + g(snd p)
          in
              h(fst p)
          end
      end

    in
      add(1, add(88, 12))
    end
  |};

  [%expect {|
    101
    101
    101
    101
    101 |}]

let%expect_test "add6" =
  Test.run {|
    let add(p : int * (int * (int * int))) : int = (fst p) + ((fst (snd p)) + (fst (snd (snd p))))
    in
      add(1, (88, (12, 10000)))
    end
  |};

  [%expect {|
    101
    101
    101
    101
    101 |}]

let%expect_test "add7" =
  Test.run {|
    let x : int ref = ref 100
    in let y : int ref = ref 1
      in
          !x + !y
      end
    end
  |};

  [%expect {|
    101
    101
    101
    101
    101 |}]
