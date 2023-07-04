program test_validate
    use, intrinsic :: iso_fortran_env
    use :: strith_validate
    use :: strith_parameter
    use :: strith_arithmetic_op_unary_negate
    implicit none
    character(:), allocatable :: str

    str = weights_of_digits(10)
    if (validate(str) .eqv. .true.) then
        print *, "PASSED: "//str//" is valid."
    else
        print *, "FAILED: "//str//" is not valid."
    end if

    str = -weights_of_digits(75)
    if (validate(str) .eqv. .true.) then
        print *, "PASSED: "//str//" is valid."
    else
        print *, "FAILED: "//str//" is not valid."
    end if

    str = weights_of_digits(20)
    str(15:15) = "a"
    if (validate(str) .eqv. .false.) then
        print *, "PASSED: "//str//" is not valid."
    else
        print *, "FAILED: expected "//str//" is not valid but validate() estimate it is valid."
    end if

    str = weights_of_digits(100)
    str(4:4) = "+"
    if (validate(str) .eqv. .false.) then
        print *, "PASSED: "//str//" is not valid."
    else
        print *, "FAILED: expected "//str//" is not valid but validate() estimate it is valid."
    end if

    str = weights_of_digits(50)
    str(sign_index:sign_index) = "0"
    if (validate(str) .eqv. .false.) then
        print *, "PASSED: "//str//" is not valid."
    else
        print *, "FAILED: expected "//str//" is not valid but validate() estimate it is valid."
    end if
end program test_validate
