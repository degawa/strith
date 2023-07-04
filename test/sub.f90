program test_sub
    use, intrinsic :: iso_fortran_env
    use :: strith_parameter
    use :: strith_arithmetic_basic_core_sub
    implicit none

    character(len=digits) :: a, b, add
    character(:), allocatable :: expr

    add = sub_core(weights_of_digits(4), weights_of_digits(3))
    expr = "16-8=8"
    if (add == "+000000000000000000000000000000000000008") then
        print *, "PASSED: ", expr
    else
        print *, "FAILED: expected "//expr//", but got ", add
    end if

    a = "+000000000000000000000000152125131763605"
    b = "+000000000000000000000000007420738134810"
    add = sub_core(a, b)
    expr = "152125131763605+7420738134810=144704393628795"
    if (add == "+000000000000000000000000144704393628795") then
        print *, "PASSED: ", expr
    else
        print *, "FAILED: expected "//expr//", but got ", add
    end if
end program test_sub
