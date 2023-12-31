program test_equal
    use, intrinsic :: iso_fortran_env
    use :: strith_parameter
    use :: strith_comparision_equal
    use :: strith_arithmetic_unary_negate
    implicit none

    if ((weights_of_digits(3) .stritheq.weights_of_digits(3))) then
        print '(A)', "✅PASSED: 8==8 is .true."
    else
        print '(A)', "🔥FAILED: expect that 8==8 is .true., but got .false."
    end if

    if ((weights_of_digits(3) .stritheq.weights_of_digits(4)) .eqv. .false.) then
        print '(A)', "✅PASSED: 8==16 is .false."
    else
        print '(A)', "🔥FAILED: expect that 8==16 is .false., but got .true."
    end if

    if ((weights_of_digits(4) .stritheq.-weights_of_digits(4)) .eqv. .false.) then
        print '(A)', "✅PASSED: 16==-16 is .false."
    else
        print '(A)', "🔥FAILED: expect that 16==-16 is .false., but got .true."
    end if

    if ((weights_of_digits(4) .strithne.-weights_of_digits(4))) then
        print '(A)', "✅PASSED: 16/=-16 is .true."
    else
        print '(A)', "🔥FAILED: expect that 16/=-16 is .true., but got .false."
    end if

    if ((weights_of_digits(3) .strithne.weights_of_digits(3)) .eqv. .false.) then
        print '(A)', "✅PASSED: 8/=8 is .true."
    else
        print '(A)', "🔥FAILED: expect that 8/=8 is .true., but got .false."
    end if
end program test_equal
