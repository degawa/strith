program test_removeZeroPadding
    use, intrinsic :: iso_fortran_env
    use :: strith_util_removeZeroPadding
    use :: strith_parameter
    use :: strith_arithmetic_unary_negate
    implicit none

    character(:), allocatable :: str
    character(len=digits) :: a

    a = weights_of_digits(5)
    str = remove_zero_padding(a)
    if (str == "32") then
        print '(A)', "✅PASSED: remove zero padding from positive value"
    else
        print '(A)', "🔥FAILED: remove zero padding from positive value. expected 32, but got ", str
    end if

    a = -weights_of_digits(8)
    str = remove_zero_padding(a)
    if (str == "-256") then
        print '(A)', "✅PASSED: remove zero padding from negative value"
    else
        print '(A)', "🔥FAILED: remove zero padding from negative value. expected -256, but got ", str
    end if

    a = zero
    str = remove_zero_padding(a)
    if (str == "0") then
        print '(A)', "✅PASSED: remove zero padding from positive zero"
    else
        print '(A)', "🔥FAILED: remove zero padding from positive zero. expected 0, but got ", str
    end if

    a = -zero
    str = remove_zero_padding(a)
    if (str == "-0") then
        print '(A)', "✅PASSED: remove zero padding from negative zero"
    else
        print '(A)', "🔥FAILED: remove zero padding from negative zero. expected 0, but got ", str
    end if
end program test_removeZeroPadding
