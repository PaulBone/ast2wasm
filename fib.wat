(module $my-module
  (func (export "fib") (param i32) (result i32)
    (get_local 0)
    (get_local 0)
    (i32.add)
    (return)
  )
)

