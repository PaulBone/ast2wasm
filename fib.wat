(module $my-module
  (func $fib (export "fib") (param i32) (result i32)
    (get_local 0)
    (i32.const 2)
    (i32.ge_u)
    (if (result i32)
        (then (get_local 0)
            (i32.const 1)
            (i32.sub)
            (call $fib)
            (get_local 0)
            (i32.const 2)
            (i32.sub)
            (call $fib)
            (i32.add)
            (return))
        (else
            (i32.const 1)
            (return)))
  )

  (func $fibexpr (export "fibexpr") (param i32) (result i32)
    (if (result i32) (i32.ge_u (get_local 0) (i32.const 2))
        (then (i32.add
            (call $fib (i32.sub (get_local 0) (i32.const 1)))
            (call $fib (i32.sub (get_local 0) (i32.const 2)))))
        (else (i32.const 1))))
)

