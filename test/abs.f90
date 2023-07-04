program test_abs
    use :: strith_arithmetic_op_unary_abs
    use :: strith_arithmetic_op_unary_negate
    use :: strith_parameter
    implicit none

    character(:), allocatable :: str

    str = -weights_of_digits(8)
    str = abs(str)
    if (str == "+000000000000000000000000000000000000256") then
        print *, "PASSED: abs(negative value) returns the positive value"
    else
        print *, "FAILED: abs(negative value) returns the positive value. "// &
            "expected +000000000000000000000000000000000000256 but got "//str
    end if

    str = weights_of_digits(8)
    if (str == "+000000000000000000000000000000000000256") then
        print *, "PASSED: abs(positive value) returns the positive value"
    else
        print *, "FAILED: abs(positive value) returns the positive value. "// &
            "expected +000000000000000000000000000000000000256 but got "//str
    end if
end program test_abs
