let%expect_test "while" = 
  Test.run {|
    let i : int ref = ref (-100)
    in
      begin
        while !i < 3 do
          i := (!i + 1)
        end;
        !i
      end
    end
  |};

  [%expect {|
    3
    3
    3
    3
    3 |}]
