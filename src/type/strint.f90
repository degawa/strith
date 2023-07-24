module strith_type_strint
    use, intrinsic :: iso_fortran_env
    use :: strith_parameter
    implicit none
    private
    public :: new_strint
    public :: abs

    type, public :: strint_type
        character(len=digits), private :: str = zero
    contains
        procedure, public, pass :: to_string

        procedure, public, pass :: add
        procedure, public, pass :: sub
        procedure, public, pass :: negate
        generic :: operator(+) => add
        generic :: operator(-) => sub, negate

        procedure, public, pass :: equal
        procedure, public, pass :: not_equal
        procedure, public, pass :: greater_than
        procedure, public, pass :: greater_than_or_equal_to
        procedure, public, pass :: less_than
        procedure, public, pass :: less_than_or_equal_to
        generic :: operator(==) => equal
        generic :: operator(/=) => not_equal
        generic :: operator(>) => greater_than
        generic :: operator(>=) => greater_than_or_equal_to
        generic :: operator(<) => less_than
        generic :: operator(<=) => less_than_or_equal_to
    end type strint_type

    interface new_strint
        procedure :: construct_strint_i8
        procedure :: construct_strint_i16
        procedure :: construct_strint_i32
        procedure :: construct_strint_i64
        procedure :: construct_strint_str
    end interface

    interface abs
        procedure :: abs_strint
    end interface
contains
    pure elemental function construct_strint_i8(i8) result(strint)
        use :: strith_util_toString
        implicit none
        integer(int8), intent(in) :: i8
        type(strint_type) :: strint

        strint%str = to_string(i8)
    end function construct_strint_i8

    pure elemental function construct_strint_i16(i16) result(strint)
        use :: strith_util_toString
        implicit none
        integer(int16), intent(in) :: i16
        type(strint_type) :: strint

        strint%str = to_string(i16)
    end function construct_strint_i16

    pure elemental function construct_strint_i32(i32) result(strint)
        use :: strith_util_toString
        implicit none
        integer(int32), intent(in) :: i32
        type(strint_type) :: strint

        strint%str = to_string(i32)
    end function construct_strint_i32

    pure elemental function construct_strint_i64(i64) result(strint)
        use :: strith_util_toString
        implicit none
        integer(int64), intent(in) :: i64
        type(strint_type) :: strint

        strint%str = to_string(i64)
    end function construct_strint_i64

    pure elemental function construct_strint_str(str) result(strint)
        use :: strith_util_isValid
        implicit none
        character(len=digits), intent(in) :: str
        type(strint_type) :: strint

        if (is_valid(str)) then
            strint%str = str
        else
        end if
    end function construct_strint_str

    pure function to_string(a, remove_0_padding) result(str)
        use :: strith_util_removeZeroPadding
        implicit none
        class(strint_type), intent(in) :: a
        logical, intent(in), optional :: remove_0_padding
        character(:), allocatable :: str

        str = a%str

        if (present(remove_0_padding)) then
            if (remove_0_padding) str = remove_zero_padding(str)
        end if
    end function to_string

    pure elemental function add(a, b) result(c)
        use :: strith_arithmetic_basic_add
        implicit none
        class(strint_type), intent(in) :: a
        class(strint_type), intent(in) :: b
        type(strint_type) :: c

        c%str = a%str + b%str
    end function add

    pure elemental function sub(a, b) result(c)
        use :: strith_arithmetic_basic_sub
        implicit none
        class(strint_type), intent(in) :: a
        class(strint_type), intent(in) :: b
        type(strint_type) :: c

        c%str = a%str - b%str
    end function sub

    pure elemental function negate(a) result(minus_a)
        use :: strith_arithmetic_unary_negate
        implicit none
        class(strint_type), intent(in) :: a
        type(strint_type) :: minus_a

        minus_a%str = -a%str
    end function negate

    pure elemental logical function equal(a, b)
        use :: strith_comparision_equal
        implicit none
        class(strint_type), intent(in) :: a
        class(strint_type), intent(in) :: b

        equal = (a%str.stritheq.b%str)
    end function equal

    pure elemental logical function not_equal(a, b)
        use :: strith_comparision_equal
        implicit none
        class(strint_type), intent(in) :: a
        class(strint_type), intent(in) :: b

        not_equal = (a%str.strithne.b%str)
    end function not_equal

    pure elemental logical function greater_than(a, b)
        use :: strith_comparision_greater
        implicit none
        class(strint_type), intent(in) :: a
        class(strint_type), intent(in) :: b

        greater_than = (a%str.strithgt.b%str)
    end function greater_than

    pure elemental logical function greater_than_or_equal_to(a, b)
        use :: strith_comparision_greater
        implicit none
        class(strint_type), intent(in) :: a
        class(strint_type), intent(in) :: b

        greater_than_or_equal_to = (a%str.strithge.b%str)
    end function greater_than_or_equal_to

    pure elemental logical function less_than(a, b)
        use :: strith_comparision_less
        implicit none
        class(strint_type), intent(in) :: a
        class(strint_type), intent(in) :: b

        less_than = (a%str.strithlt.b%str)
    end function less_than

    pure elemental logical function less_than_or_equal_to(a, b)
        use :: strith_comparision_less
        implicit none
        class(strint_type), intent(in) :: a
        class(strint_type), intent(in) :: b

        less_than_or_equal_to = (a%str.strithle.b%str)
    end function less_than_or_equal_to

    pure elemental function abs_strint(a) result(abs_a)
        use :: strith_arithmetic_unary_abs
        implicit none
        class(strint_type), intent(in) :: a
        type(strint_type) :: abs_a

        abs_a%str = abs(a%str)
    end function abs_strint
end module strith_type_strint
