program test_sign
    use :: strith_arithmetic_unary_sign
    use :: strith_arithmetic_unary_negate
    use :: strith_parameter
    implicit none

    character(:), allocatable :: str
    character(1) :: str_sign

    str = zero
    str_sign = sign(str)
    if (str_sign == plus_sign) then
        print '(A)', "✅PASSED: sign(+0) returns '+' sign symbol."
    else
        print '(A)', "🔥sign(+0) returns '+' sign symbol."// &
            "expected '+' but got "//str_sign
    end if

    str = one
    str_sign = sign(str)
    if (str_sign == plus_sign) then
        print '(A)', "✅PASSED: sign(+1) returns '+' sign symbol."
    else
        print '(A)', "🔥sign(+1) returns '+' sign symbol."// &
            "expected '+' but got "//str_sign
    end if

    str = weights_of_digits(127)
    str_sign = sign(str)
    if (str_sign == plus_sign) then
        print '(A)', "✅PASSED: sign(+170141183460469231731687303715884105728) returns '+' sign symbol."
    else
        print '(A)', "🔥sign(+170141183460469231731687303715884105728) returns '+' sign symbol."// &
            "expected '+' but got "//str_sign
    end if

    str = -zero
    str_sign = sign(str)
    if (str_sign == minus_sign) then
        print '(A)', "✅PASSED: sign(-0) returns '-' sign symbol."
    else
        print '(A)', "🔥sign(-0) returns '-' sign symbol."// &
            "expected '-' but got "//str_sign
    end if

    str = -one
    str_sign = sign(str)
    if (str_sign == minus_sign) then
        print '(A)', "✅PASSED: sign(-1) returns '-' sign symbol."
    else
        print '(A)', "🔥sign(-1) returns '-' sign symbol."// &
            "expected '-' but got "//str_sign
    end if

    str = -weights_of_digits(127)
    str_sign = sign(str)
    if (str_sign == minus_sign) then
        print '(A)', "✅PASSED: sign(-170141183460469231731687303715884105728) returns '-' sign symbol."
    else
        print '(A)', "🔥sign(-170141183460469231731687303715884105728) returns '-' sign symbol."// &
            "expected '-' but got "//str_sign
    end if
end program test_sign
