open OUnit2

let test_it_works _ = assert_equal (1 + 1) 2

let suite = "Suite1" >::: [ "test_it_works" >:: test_it_works ]

let () = run_test_tt_main suite
