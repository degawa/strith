program test_less
    use :: strith_parameter
    use :: strith_arithmetic_comparision_core_less
    use :: strith_arithmetic_comparision_less
    use :: strith_arithmetic_op_unary_negate
    implicit none

    if (is_less_without_considering_sign(weights_of_digits(3), weights_of_digits(4))) then
        print *, "PASSED: 8<16 is .true."
    else
        print *, "FAILED: expect that 8<16 is .true., but got .false."
    end if

    if (is_less_without_considering_sign(weights_of_digits(4), weights_of_digits(3)) .eqv. .false.) then
        print *, "PASSED: 16<8 is .false."
    else
        print *, "FAILED: expect that 16<8 is .false., but got .true."
    end if

    if (is_less_without_considering_sign(weights_of_digits(3), weights_of_digits(3)) .eqv. .false.) then
        print *, "PASSED: 8<8 is .false."
    else
        print *, "FAILED: expect that 8<8 is .false., but got .true."
    end if

    if (is_less_without_considering_sign(-weights_of_digits(4), -weights_of_digits(3)) .eqv. .false.) then
        print *, "PASSED: -16<-8 is .false. because sign is not considered"
    else
        print *, "FAILED: expect that -16<-8 is .false., but got .true."
    end if

    if (is_less_without_considering_sign(-weights_of_digits(3), -weights_of_digits(4)) .eqv. .true.) then
        print *, "PASSED: -8<-16 is .true. because sign is not considered"
    else
        print *, "FAILED: expect that -8<-16 is .true., but got .false."
    end if

    if (is_less_without_considering_sign(-weights_of_digits(3), -weights_of_digits(3)) .eqv. .false.) then
        print *, "PASSED: -8<-8 is .false."
    else
        print *, "FAILED: expect that -8<-8 is .false., but got .true."
    end if

    !---
    if ((weights_of_digits(3) .strlt.weights_of_digits(4))) then
        print *, "PASSED: 8<16 is .true."
    else
        print *, "FAILED: expect that 8<16 is .true., but got .false."
    end if

    if ((weights_of_digits(4) .strlt.weights_of_digits(3)) .eqv. .false.) then
        print *, "PASSED: 16<8 is .false."
    else
        print *, "FAILED: expect that 16<8 is .false., but got .true."
    end if

    if ((weights_of_digits(3) .strlt.weights_of_digits(3)) .eqv. .false.) then
        print *, "PASSED: 8<8 is .false."
    else
        print *, "FAILED: expect that 8<8 is .false., but got .true."
    end if

    if ((-weights_of_digits(4) .strlt.-weights_of_digits(3)) .eqv. .true.) then
        print *, "PASSED: -16<-8 is .true."
    else
        print *, "FAILED: expect that -16<-8 is .true., but got .false."
    end if

    if ((-weights_of_digits(3) .strlt.-weights_of_digits(4)) .eqv. .false.) then
        print *, "PASSED: -8<-16 is .false."
    else
        print *, "FAILED: expect that -8<-16 is .false., but got .true."
    end if

    if ((-weights_of_digits(3) .strlt.-weights_of_digits(3)) .eqv. .false.) then
        print *, "PASSED: -8<-8 is .false."
    else
        print *, "FAILED: expect that -8<-8 is .false., but got .true."
    end if

    !---
    if ((weights_of_digits(3) .strle.weights_of_digits(4))) then
        print *, "PASSED: 8<=16 is .true."
    else
        print *, "FAILED: expect that 8<=16 is .true., but got .false."
    end if

    if ((weights_of_digits(4) .strle.weights_of_digits(3)) .eqv. .false.) then
        print *, "PASSED: 16<=8 is .false."
    else
        print *, "FAILED: expect that 16<=8 is .false., but got .true."
    end if

    if ((weights_of_digits(3) .strle.weights_of_digits(3)) .eqv. .true.) then
        print *, "PASSED: 8<=8 is .true."
    else
        print *, "FAILED: expect that 8<=8 is .true., but got .false."
    end if

    if ((-weights_of_digits(4) .strle.-weights_of_digits(3)) .eqv. .true.) then
        print *, "PASSED: -16<=-8 is .true."
    else
        print *, "FAILED: expect that -16<=-8 is .true., but got .false."
    end if

    if ((-weights_of_digits(3) .strle.-weights_of_digits(4)) .eqv. .false.) then
        print *, "PASSED: -8<=-16 is .false."
    else
        print *, "FAILED: expect that -8<=-16 is .false., but got .true."
    end if

    if ((-weights_of_digits(3) .strle.-weights_of_digits(3)) .eqv. .true.) then
        print *, "PASSED: -8<=-8 is .true."
    else
        print *, "FAILED: expect that -8<=-8 is .true., but got .false."
    end if
end program test_less
