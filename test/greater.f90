program test_greater
    use :: strith_parameter
    use :: strith_comparision_core_greater
    use :: strith_comparision_greater
    use :: strith_arithmetic_unary_negate
    implicit none

    if (is_greater_without_considering_sign(weights_of_digits(3), weights_of_digits(2))) then
        print *, "PASSED: 8>4 is .true."
    else
        print *, "FAILED: expect that 8>4 is .true., but got .false."
    end if

    if (is_greater_without_considering_sign(weights_of_digits(2), weights_of_digits(3)) .eqv. .false.) then
        print *, "PASSED: 4>8 is .false."
    else
        print *, "FAILED: expect that 4>8 is .false., but got .true."
    end if

    if (is_greater_without_considering_sign(weights_of_digits(3), weights_of_digits(3)) .eqv. .false.) then
        print *, "PASSED: 8>8 is .false."
    else
        print *, "FAILED: expect that 8>8 is .false., but got .true."
    end if

    if (is_greater_without_considering_sign(-weights_of_digits(2), -weights_of_digits(3)) .eqv. .false.) then
        print *, "PASSED: -4>-8 is .false. because sign is not considered"
    else
        print *, "FAILED: expect that -4>-8 is .false., but got .true."
    end if

    if (is_greater_without_considering_sign(-weights_of_digits(3), -weights_of_digits(2)) .eqv. .true.) then
        print *, "PASSED: -8>-4 is .true. because sign is not considered"
    else
        print *, "FAILED: expect that -8>-4 is .true., but got .false."
    end if

    if (is_greater_without_considering_sign(-weights_of_digits(3), -weights_of_digits(3)) .eqv. .false.) then
        print *, "PASSED: -8>-8 is .false."
    else
        print *, "FAILED: expect that -8>-8 is .false., but got .true."
    end if

    !---
    if ((weights_of_digits(3) .strithgt.weights_of_digits(2))) then
        print *, "PASSED: 8>4 is .true."
    else
        print *, "FAILED: expect that 8>4 is .true., but got .false."
    end if

    if ((weights_of_digits(2) .strithgt.weights_of_digits(3)) .eqv. .false.) then
        print *, "PASSED: 4>8 is .false."
    else
        print *, "FAILED: expect that 4>8 is .false., but got .true."
    end if

    if ((weights_of_digits(3) .strithgt.weights_of_digits(3)) .eqv. .false.) then
        print *, "PASSED: 8>8 is .false."
    else
        print *, "FAILED: expect that 8>8 is .false., but got .true."
    end if

    if ((-weights_of_digits(2) .strithgt.-weights_of_digits(3)) .eqv. .true.) then
        print *, "PASSED: -4>-8 is .true."
    else
        print *, "FAILED: expect that -4>-8 is .true., but got .false."
    end if

    if ((-weights_of_digits(3) .strithgt.-weights_of_digits(2)) .eqv. .false.) then
        print *, "PASSED: -8>-4 is .false."
    else
        print *, "FAILED: expect that -8>-4 is .false., but got .true."
    end if

    if ((-weights_of_digits(3) .strithgt.-weights_of_digits(3)) .eqv. .false.) then
        print *, "PASSED: -8>-8 is .false."
    else
        print *, "FAILED: expect that -8>-8 is .false., but got .true."
    end if

    !---
    if ((weights_of_digits(3) .strithge.weights_of_digits(2))) then
        print *, "PASSED: 8>=4 is .true."
    else
        print *, "FAILED: expect that 8>=4 is .true., but got .false."
    end if

    if ((weights_of_digits(2) .strithge.weights_of_digits(3)) .eqv. .false.) then
        print *, "PASSED: 4>=8 is .false."
    else
        print *, "FAILED: expect that 4>=8 is .false., but got .true."
    end if

    if ((weights_of_digits(3) .strithge.weights_of_digits(3)) .eqv. .true.) then
        print *, "PASSED: 8>=8 is .true."
    else
        print *, "FAILED: expect that 8>=8 is .true., but got .false."
    end if

    if ((-weights_of_digits(2) .strithge.-weights_of_digits(3)) .eqv. .true.) then
        print *, "PASSED: -4>=-8 is .true."
    else
        print *, "FAILED: expect that -4>=-8 is .true., but got .false."
    end if

    if ((-weights_of_digits(3) .strithge.-weights_of_digits(2)) .eqv. .false.) then
        print *, "PASSED: -8>=-4 is .false."
    else
        print *, "FAILED: expect that -8>=-4 is .false., but got .true."
    end if

    if ((-weights_of_digits(3) .strithge.-weights_of_digits(3)) .eqv. .true.) then
        print *, "PASSED: -8>=-8 is .true."
    else
        print *, "FAILED: expect that -8>=-8 is .true., but got .false."
    end if
end program test_greater
