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
)

