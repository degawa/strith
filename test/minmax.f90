program test_minmax
    use :: strith_parameter
    use :: strith_comparision_min
    use :: strith_comparision_max
    use :: strith_comparision_equal
    use :: strith_arithmetic_unary_negate
    implicit none

    character(len=digits) :: a, b, c

    ! min
    a = weights_of_digits(3)
    b = weights_of_digits(4)
    c = min(a, b)
    if (c == a) then
        print '(A)', "✅PASSED: min(8, 16) returns 8"
    else
        print '(A)', "🔥FAILED: expect that min(8, 16) returns 8, but got "//c
    end if

    a = weights_of_digits(4)
    b = weights_of_digits(3)
    c = min(a, b)
    if (c == b) then
        print '(A)', "✅PASSED: min(16, 8) returns 8"
    else
        print '(A)', "🔥FAILED: expect that min(16, 8) returns 8, but got "//c
    end if

    a = weights_of_digits(3)
    b = weights_of_digits(3)
    c = min(a, b)
    if (c == a .and. c == b) then
        print '(A)', "✅PASSED: min(8, 8) returns 8"
    else
        print '(A)', "🔥FAILED: expect that min(8, 8) returns 8, but got "//c
    end if

    a = -weights_of_digits(4)
    b = -weights_of_digits(3)
    c = min(a, b)
    if (c == a) then
        print '(A)', "✅PASSED: min(-16, -8) returns -16"
    else
        print '(A)', "🔥FAILED: expect that min(-16, -8) returns -16, but got "//c
    end if

    a = -weights_of_digits(3)
    b = -weights_of_digits(4)
    c = min(a, b)
    if (c == b) then
        print '(A)', "✅PASSED: min(-8, -16) returns -16"
    else
        print '(A)', "🔥FAILED: expect that min(-8, -16) returns -16, but got "//c
    end if

    a = -weights_of_digits(3)
    b = -weights_of_digits(3)
    c = min(a, b)
    if (c == a .and. c == b) then
        print '(A)', "✅PASSED: min(-8, -8) returns -8"
    else
        print '(A)', "🔥FAILED: expect that min(-8, -8) returns -8, but got "//c
    end if

    ! max
    a = weights_of_digits(3)
    b = weights_of_digits(2)
    c = max(a, b)
    if (c == a) then
        print '(A)', "✅PASSED: max(8, 4) returns 8"
    else
        print '(A)', "🔥FAILED: expect that max(8, 4) returns 8, but got "//c
    end if

    a = weights_of_digits(2)
    b = weights_of_digits(3)
    c = max(a, b)
    if (c == b) then
        print '(A)', "✅PASSED: max(4, 8) returns 8"
    else
        print '(A)', "🔥FAILED: expect that max(4, 8) returns 8, but got "//c
    end if

    a = weights_of_digits(3)
    b = weights_of_digits(3)
    c = max(a, b)
    if (c == b .and. c == a) then
        print '(A)', "✅PASSED: max(8, 8) returns 8"
    else
        print '(A)', "🔥FAILED: expect that max(8, 8) returns 8, but got "//c
    end if

    a = -weights_of_digits(2)
    b = -weights_of_digits(3)
    c = max(a, b)
    if (c == a) then
        print '(A)', "✅PASSED: max(-4, -8) returns -4"
    else
        print '(A)', "🔥FAILED: expect that max(-4, -8) returns -4, but got "//c
    end if

    a = -weights_of_digits(3)
    b = -weights_of_digits(2)
    c = max(a, b)
    if (c == b) then
        print '(A)', "✅PASSED: max(-8, -4) returns -4"
    else
        print '(A)', "🔥FAILED: expect that max(-8, -4) returns -4, but got "//c
    end if

    a = -weights_of_digits(3)
    b = -weights_of_digits(3)
    c = max(a, b)
    if (c == a .and. c == b) then
        print '(A)', "✅PASSED: max(-4, -4) returns -4"
    else
        print '(A)', "🔥FAILED: expect that max(-4, -4) returns -4, but got "//c
    end if
end program test_minmax
