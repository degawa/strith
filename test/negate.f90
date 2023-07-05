program test_negate
    use :: strith_parameter
    use :: strith_arithmetic_unary_negate
    implicit none

    character(:), allocatable :: str

    str = -weights_of_digits(1)
    if (str == "-000000000000000000000000000000000000002") then
        print *, "PASSED: negate of positive value"
    else
        print *, "FAILED: negate of positive value. expected -000000000000000000000000000000000000002 but got"//str
    end if

    str = -str
    if (str == "+000000000000000000000000000000000000002") then
        print *, "PASSED: negate of negative value"
    else
        print *, "FAILED: negate of negative value. expected +000000000000000000000000000000000000002 but got"//str
    end if
end program test_negate
