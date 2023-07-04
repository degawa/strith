program test_equal
    use, intrinsic :: iso_fortran_env
    use :: strith_parameter
    use :: strith_arithmetic_comparision_equal
    use :: strith_arithmetic_op_unary_negate
    implicit none

    if (is_equal(weights_of_digits(3), weights_of_digits(3))) then
        print *, "PASSED: 8==8 is .true."
    else
        print *, "FAILED: expect that 8==8 is .true., but got .false."
    end if

    if (is_equal(weights_of_digits(3), weights_of_digits(4)) .eqv. .false.) then
        print *, "PASSED: 8==16 is .false."
    else
        print *, "FAILED: expect that 8==16 is .false., but got .true."
    end if

    if (is_equal(weights_of_digits(4), -weights_of_digits(4)) .eqv. .false.) then
        print *, "PASSED: 16==-16 is .false."
    else
        print *, "FAILED: expect that 16==-16 is .false., but got .true."
    end if

    if (is_not_equal(weights_of_digits(4), -weights_of_digits(4))) then
        print *, "PASSED: 16/=-16 is .true."
    else
        print *, "FAILED: expect that 16/=-16 is .true., but got .false."
    end if

    if (is_not_equal(weights_of_digits(3), weights_of_digits(3)) .eqv. .false.) then
        print *, "PASSED: 8/=8 is .true."
    else
        print *, "FAILED: expect that 8/=8 is .true., but got .false."
    end if
end program test_equal