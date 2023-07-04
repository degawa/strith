program test_sub
    use, intrinsic :: iso_fortran_env
    use :: strith_parameter
    use :: strith_arithmetic_basic_core_sub
    use :: strith_arithmetic_basic_sub
    implicit none

    character(len=digits) :: a, b, sub
    character(:), allocatable :: expr

    sub = sub_core(weights_of_digits(4), weights_of_digits(3))
    expr = "16-8=8"
    if (sub == "+000000000000000000000000000000000000008") then
        print *, "PASSED: ", expr
    else
        print *, "FAILED: expected "//expr//", but got ", sub
    end if

    a = "+000000000000000000000000152125131763605"
    b = "+000000000000000000000000007420738134810"
    sub = sub_core(a, b)
    expr = "152125131763605+7420738134810=144704393628795"
    if (sub == "+000000000000000000000000144704393628795") then
        print *, "PASSED: ", expr
    else
        print *, "FAILED: expected "//expr//", but got ", sub
    end if
end program test_sub
