program test_less
    use :: strith_parameter
    use :: strith_comparision_core_less
    use :: strith_comparision_less
    use :: strith_arithmetic_unary_negate
    implicit none

    if (is_less_without_considering_sign(weights_of_digits(3), weights_of_digits(4))) then
        print '(A)', "✅PASSED: 8<16 is .true."
    else
        print '(A)', "🔥FAILED: expect that 8<16 is .true., but got .false."
    end if

    if (is_less_without_considering_sign(weights_of_digits(4), weights_of_digits(3)) .eqv. .false.) then
        print '(A)', "✅PASSED: 16<8 is .false."
    else
        print '(A)', "🔥FAILED: expect that 16<8 is .false., but got .true."
    end if

    if (is_less_without_considering_sign(weights_of_digits(3), weights_of_digits(3)) .eqv. .false.) then
        print '(A)', "✅PASSED: 8<8 is .false."
    else
        print '(A)', "🔥FAILED: expect that 8<8 is .false., but got .true."
    end if

    if (is_less_without_considering_sign(-weights_of_digits(4), -weights_of_digits(3)) .eqv. .false.) then
        print '(A)', "✅PASSED: -16<-8 is .false. because sign is not considered"
    else
        print '(A)', "🔥FAILED: expect that -16<-8 is .false., but got .true."
    end if

    if (is_less_without_considering_sign(-weights_of_digits(3), -weights_of_digits(4)) .eqv. .true.) then
        print '(A)', "✅PASSED: -8<-16 is .true. because sign is not considered"
    else
        print '(A)', "🔥FAILED: expect that -8<-16 is .true., but got .false."
    end if

    if (is_less_without_considering_sign(-weights_of_digits(3), -weights_of_digits(3)) .eqv. .false.) then
        print '(A)', "✅PASSED: -8<-8 is .false."
    else
        print '(A)', "🔥FAILED: expect that -8<-8 is .false., but got .true."
    end if

    !---
    if ((weights_of_digits(3) .strithlt.weights_of_digits(4))) then
        print '(A)', "✅PASSED: 8<16 is .true."
    else
        print '(A)', "🔥FAILED: expect that 8<16 is .true., but got .false."
    end if

    if ((weights_of_digits(4) .strithlt.weights_of_digits(3)) .eqv. .false.) then
        print '(A)', "✅PASSED: 16<8 is .false."
    else
        print '(A)', "🔥FAILED: expect that 16<8 is .false., but got .true."
    end if

    if ((weights_of_digits(3) .strithlt.weights_of_digits(3)) .eqv. .false.) then
        print '(A)', "✅PASSED: 8<8 is .false."
    else
        print '(A)', "🔥FAILED: expect that 8<8 is .false., but got .true."
    end if

    if ((-weights_of_digits(4) .strithlt.-weights_of_digits(3)) .eqv. .true.) then
        print '(A)', "✅PASSED: -16<-8 is .true."
    else
        print '(A)', "🔥FAILED: expect that -16<-8 is .true., but got .false."
    end if

    if ((-weights_of_digits(3) .strithlt.-weights_of_digits(4)) .eqv. .false.) then
        print '(A)', "✅PASSED: -8<-16 is .false."
    else
        print '(A)', "🔥FAILED: expect that -8<-16 is .false., but got .true."
    end if

    if ((-weights_of_digits(3) .strithlt.-weights_of_digits(3)) .eqv. .false.) then
        print '(A)', "✅PASSED: -8<-8 is .false."
    else
        print '(A)', "🔥FAILED: expect that -8<-8 is .false., but got .true."
    end if

    !---
    if ((weights_of_digits(3) .strithle.weights_of_digits(4))) then
        print '(A)', "✅PASSED: 8<=16 is .true."
    else
        print '(A)', "🔥FAILED: expect that 8<=16 is .true., but got .false."
    end if

    if ((weights_of_digits(4) .strithle.weights_of_digits(3)) .eqv. .false.) then
        print '(A)', "✅PASSED: 16<=8 is .false."
    else
        print '(A)', "🔥FAILED: expect that 16<=8 is .false., but got .true."
    end if

    if ((weights_of_digits(3) .strithle.weights_of_digits(3)) .eqv. .true.) then
        print '(A)', "✅PASSED: 8<=8 is .true."
    else
        print '(A)', "🔥FAILED: expect that 8<=8 is .true., but got .false."
    end if

    if ((-weights_of_digits(4) .strithle.-weights_of_digits(3)) .eqv. .true.) then
        print '(A)', "✅PASSED: -16<=-8 is .true."
    else
        print '(A)', "🔥FAILED: expect that -16<=-8 is .true., but got .false."
    end if

    if ((-weights_of_digits(3) .strithle.-weights_of_digits(4)) .eqv. .false.) then
        print '(A)', "✅PASSED: -8<=-16 is .false."
    else
        print '(A)', "🔥FAILED: expect that -8<=-16 is .false., but got .true."
    end if

    if ((-weights_of_digits(3) .strithle.-weights_of_digits(3)) .eqv. .true.) then
        print '(A)', "✅PASSED: -8<=-8 is .true."
    else
        print '(A)', "🔥FAILED: expect that -8<=-8 is .true., but got .false."
    end if
end program test_less
