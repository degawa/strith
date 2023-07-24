program test_toStringUdt
    use, intrinsic :: iso_fortran_env
    use :: strith_util_toString
    use :: strith_parameter
    implicit none

    character(:), allocatable :: str
    character(:), allocatable :: testname

    str = to_string(123456, do_nothing)
    testname = "to_string_udt with a callback procedure `do_nothing` that do nothing return 0"
    if (str == "+000000000000000000000000000000000000000") then
        print '(A)', "✅PASSED: "//testname
    else
        print '(A)', "🔥FAILED: "//testname// &
            "expected +000000000000000000000000000000000000000 but got "//str
    end if

    str = to_string(123456, return_minus_zero)
    testname = "to_string_udt with a callback procedure `return_minus_zero` that set -0 to the intent(inout) variable return 0"
    if (str == "-000000000000000000000000000000000000000") then
        print '(A)', "✅PASSED: "//testname
    else
        print '(A)', "🔥FAILED: "//testname// &
            "expected -000000000000000000000000000000000000000 but got "//str
    end if

    str = to_string(123456, i32_to_string)
    testname = "to_string_udt with a callback procedure `i32_to_string` that convert int32 to string return int32 in string"
    if (str == "+000000000000000000000000000000000123456") then
        print '(A)', "✅PASSED: "//testname
    else
        print '(A)', "🔥FAILED: "//testname// &
            "expected +000000000000000000000000000000000123456 but got "//str
    end if

    str = to_string(123456, i32_to_string, remove_0_padding=.true.)
    testname = "to_string_udt with a callback procedure `i32_to_string` that convert int32 to string with remove 0 padding &
               &return int32 in string without 0 padding"
    if (str == "123456") then
        print '(A)', "✅PASSED: "//testname
    else
        print '(A)', "🔥FAILED: "//testname// &
            "expected 123456 but got"//str
    end if

    str = to_string(int(B"10000000000000000000000000000000"), i32_to_string_unsigned, as_unsigned=.true.)
    testname = "to_string_udt with a callback procedure `i32_to_string_unsigned` that convert int32 &
               &to string with remove 0 padding return unsigned int32 in string without 0 padding"
    if (str == "+000000000000000000000000000002147483648") then
        print '(A)', "✅PASSED: "//testname
    else
        print '(A)', "🔥FAILED: "//testname// &
            "expected +000000000000000000000000000002147483648 but got "//str
    end if

    str = to_string(int(B"10000000000000000000000000000000"), i32_to_string_unsigned, as_unsigned=.false.)
    testname = "to_string_udt with a callback procedure `i32_to_string_unsigned` that convert int32 &
    &to string with remove 0 padding return signed int32 in string without 0 padding"
    if (str == "-000000000000000000000000000002147483648") then
        print '(A)', "✅PASSED: "//testname
    else
        print '(A)', "🔥FAILED: "//testname// &
            "expected -000000000000000000000000000002147483648 but got "//str
    end if
contains
    pure subroutine do_nothing(var, as_unsigned, str)
        implicit none
        class(*), intent(in) :: var
        logical, intent(in) :: as_unsigned
        character(len=digits), intent(inout) :: str
    end subroutine do_nothing

    pure subroutine return_minus_zero(var, as_unsigned, str)
        use :: strith_arithmetic_unary_negate
        implicit none
        class(*), intent(in) :: var
        logical, intent(in) :: as_unsigned
        character(len=digits), intent(inout) :: str
        str = -zero
    end subroutine return_minus_zero

    pure subroutine i32_to_string(var, as_unsigned, str)
        use :: strith_arithmetic_unary_negate
        use :: strith_arithmetic_basic_add
        use :: strith_arithmetic_basic_sub
        implicit none
        class(*), intent(in) :: var
        logical, intent(in) :: as_unsigned
        character(len=digits), intent(inout) :: str

        integer(int32) :: i, bit
        select type (var); type is (integer(int32))
            do i = 0, bit_size(var) - 2
                bit = ibits(var, pos=i, len=1)
                if (bit == 1) then
                    str = weights_of_digits(i) + str
                end if
            end do
            ! most significant bit
            i = bit_size(var) - 1
            bit = ibits(var, pos=i, len=1)
            if (bit == 1) str = str - weights_of_digits(i)
        end select
    end subroutine i32_to_string

    pure subroutine i32_to_string_unsigned(var, as_unsigned, str)
        use :: strith_arithmetic_unary_negate
        use :: strith_arithmetic_basic_add
        use :: strith_arithmetic_basic_sub
        implicit none
        class(*), intent(in) :: var
        logical, intent(in) :: as_unsigned
        character(len=digits), intent(inout) :: str

        integer(int32) :: i, bit
        select type (var); type is (integer(int32))
            do i = 0, bit_size(var) - 2
                bit = ibits(var, pos=i, len=1)
                if (bit == 1) then
                    str = weights_of_digits(i) + str
                end if
            end do
            ! most significant bit
            i = bit_size(var) - 1
            bit = ibits(var, pos=i, len=1)
            if (as_unsigned) then
                if (bit == 1) str = weights_of_digits(i) + str
            else
                if (bit == 1) str = str - weights_of_digits(i)
            end if
        end select
    end subroutine i32_to_string_unsigned
end program test_toStringUdt
