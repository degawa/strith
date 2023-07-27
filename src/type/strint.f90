module strith_type_strint
    use, intrinsic :: iso_fortran_env
    use :: strith_parameter
    use :: strith_util_isValid
    implicit none
    private
    public :: new_strint
    public :: abs
    public :: min, max
    public :: assignment(=)

    type, public :: strint_type
        character(len=digits), private :: str = zero
    contains
        procedure, public, pass(a) :: to_string

        procedure, public, pass(a) :: add
        procedure, public, pass(a) :: sub
        procedure, public, pass(a) :: negate
        procedure, public, pass(a) :: add_str
        procedure, public, pass(b) :: r_add_str
        procedure, public, pass(a) :: sub_str
        procedure, public, pass(b) :: r_sub_str
        generic :: operator(+) => add, add_str, r_add_str
        generic :: operator(-) => sub, sub_str, r_sub_str, &
                                  negate !&

        procedure, public, pass(a) :: equal
        procedure, public, pass(a) :: not_equal
        procedure, public, pass(a) :: greater_than
        procedure, public, pass(a) :: greater_than_or_equal_to
        procedure, public, pass(a) :: less_than
        procedure, public, pass(a) :: less_than_or_equal_to
        procedure, public, pass(a) :: equal_str
        procedure, public, pass(b) :: r_equal_str
        procedure, public, pass(a) :: not_equal_str
        procedure, public, pass(b) :: r_not_equal_str
        procedure, public, pass(a) :: greater_than_str
        procedure, public, pass(b) :: r_greater_than_str
        procedure, public, pass(a) :: greater_than_or_equal_to_str
        procedure, public, pass(b) :: r_greater_than_or_equal_to_str
        procedure, public, pass(a) :: less_than_str
        procedure, public, pass(b) :: r_less_than_str
        procedure, public, pass(a) :: less_than_or_equal_to_str
        procedure, public, pass(b) :: r_less_than_or_equal_to_str
        !&<
        generic :: operator(==) => equal, &
                                   equal_str, &
                                   r_equal_str
        generic :: operator(/=) => not_equal, &
                                   not_equal_str, &
                                   r_not_equal_str
        generic :: operator(>)  => greater_than, &
                                   greater_than_str, &
                                   r_greater_than_str
        generic :: operator(>=) => greater_than_or_equal_to, &
                                   greater_than_or_equal_to_str, &
                                   r_greater_than_or_equal_to_str
        generic :: operator(<)  => less_than, &
                                   less_than_str, &
                                   r_less_than_str
        generic :: operator(<=) => less_than_or_equal_to, &
                                   less_than_or_equal_to_str, &
                                   r_less_than_or_equal_to_str
        !&>
    end type strint_type

    interface new_strint
        procedure :: construct_strint_i8
        procedure :: construct_strint_i16
        procedure :: construct_strint_i32
        procedure :: construct_strint_i64
        procedure :: construct_strint_str
    end interface

    interface assignment(=)
        procedure :: assign_str
    end interface

    interface abs
        procedure :: abs_strint
    end interface

    interface min
        procedure :: min_strint
        procedure :: min_strint_str
        procedure :: min_str_strint
    end interface

    interface max
        procedure :: max_strint
        procedure :: max_strint_str
        procedure :: max_str_strint
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
            strint%str = zero
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

    pure elemental subroutine assign_str(lhs, rhs)
        use :: strith_util_isValid
        implicit none
        type(strint_type), intent(out) :: lhs
        character(len=digits), intent(in) :: rhs

        if (is_valid(rhs)) then
            lhs%str = rhs
        else
            lhs%str = zero
        end if
    end subroutine assign_str

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

    pure elemental function max_strint(a, b) result(max_)
        use :: strith_comparision_max
        implicit none
        class(strint_type), intent(in) :: a
        class(strint_type), intent(in) :: b
        type(strint_type) :: max_

        max_%str = max(a%str, b%str)
    end function max_strint

    pure elemental function min_strint(a, b) result(min_)
        use :: strith_comparision_min
        implicit none
        class(strint_type), intent(in) :: a
        class(strint_type), intent(in) :: b
        type(strint_type) :: min_

        min_%str = min(a%str, b%str)
    end function min_strint

    !------------------------------------------------------------------!
    pure elemental function add_str(a, b) result(c)
        use :: strith_arithmetic_basic_add
        implicit none
        class(strint_type), intent(in) :: a
        character(len=digits), intent(in) :: b
        type(strint_type) :: c

        if (is_valid(b)) then
            c%str = a%str + b
        else
            c%str = zero
        end if
    end function add_str

    pure elemental function r_add_str(a, b) result(c)
        use :: strith_arithmetic_basic_add
        implicit none
        character(len=digits), intent(in) :: a
        class(strint_type), intent(in) :: b
        type(strint_type) :: c

        if (is_valid(a)) then
            c%str = a + b%str
        else
            c%str = zero
        end if
    end function r_add_str

    pure elemental function sub_str(a, b) result(c)
        use :: strith_arithmetic_basic_sub
        implicit none
        class(strint_type), intent(in) :: a
        character(len=digits), intent(in) :: b
        type(strint_type) :: c

        if (is_valid(b)) then
            c%str = a%str - b
        else
            c%str = zero
        end if
    end function sub_str

    pure elemental function r_sub_str(a, b) result(c)
        use :: strith_arithmetic_basic_sub
        implicit none
        character(len=digits), intent(in) :: a
        class(strint_type), intent(in) :: b
        type(strint_type) :: c

        if (is_valid(a)) then
            c%str = a - b%str
        else
            c%str = zero
        end if
    end function r_sub_str

    pure elemental logical function equal_str(a, b)
        use :: strith_comparision_equal
        implicit none
        class(strint_type), intent(in) :: a
        character(len=digits), intent(in) :: b

        if (is_valid(b)) then
            equal_str = (a%str .stritheq. b) !&
        else
            equal_str = .false.
        end if
    end function equal_str

    pure elemental logical function r_equal_str(a, b)
        use :: strith_comparision_equal
        implicit none
        character(len=digits), intent(in) :: a
        class(strint_type), intent(in) :: b

        if (is_valid(a)) then
            r_equal_str = (a .stritheq. b%str) !&
        else
            r_equal_str = .false.
        end if
    end function r_equal_str

    pure elemental logical function not_equal_str(a, b)
        use :: strith_comparision_equal
        implicit none
        class(strint_type), intent(in) :: a
        character(len=digits), intent(in) :: b

        not_equal_str = .not. (a == b)
    end function not_equal_str

    pure elemental logical function r_not_equal_str(a, b)
        use :: strith_comparision_equal
        implicit none
        character(len=digits), intent(in) :: a
        class(strint_type), intent(in) :: b

        r_not_equal_str = .not. (a == b)
    end function r_not_equal_str

    pure elemental logical function greater_than_str(a, b)
        use :: strith_comparision_greater
        implicit none
        class(strint_type), intent(in) :: a
        character(len=digits), intent(in) :: b

        if (is_valid(b)) then
            greater_than_str = (a%str .strithgt. b) !&
        else
            greater_than_str = .false.
        end if
    end function greater_than_str

    pure elemental logical function r_greater_than_str(a, b)
        use :: strith_comparision_greater
        implicit none
        character(len=digits), intent(in) :: a
        class(strint_type), intent(in) :: b

        if (is_valid(a)) then
            r_greater_than_str = (a .strithgt. b%str) !&
        else
            r_greater_than_str = .false.
        end if
    end function r_greater_than_str

    pure elemental logical function greater_than_or_equal_to_str(a, b)
        use :: strith_comparision_greater
        implicit none
        class(strint_type), intent(in) :: a
        character(len=digits), intent(in) :: b

        if (is_valid(b)) then
            greater_than_or_equal_to_str = (a%str .strithge. b) !&
        else
            greater_than_or_equal_to_str = .false.
        end if
    end function greater_than_or_equal_to_str

    pure elemental logical function r_greater_than_or_equal_to_str(a, b)
        use :: strith_comparision_greater
        implicit none
        character(len=digits), intent(in) :: a
        class(strint_type), intent(in) :: b

        if (is_valid(a)) then
            r_greater_than_or_equal_to_str = (a.strithge.b%str)
        else
            r_greater_than_or_equal_to_str = .false.
        end if
    end function r_greater_than_or_equal_to_str

    pure elemental logical function less_than_str(a, b)
        use :: strith_comparision_less
        implicit none
        class(strint_type), intent(in) :: a
        character(len=digits), intent(in) :: b

        if (is_valid(b)) then
            less_than_str = (a%str .strithlt. b) !&
        else
            less_than_str = .false.
        end if
    end function less_than_str

    pure elemental logical function r_less_than_str(a, b)
        use :: strith_comparision_less
        implicit none
        character(len=digits), intent(in) :: a
        class(strint_type), intent(in) :: b

        if (is_valid(a)) then
            r_less_than_str = (a .strithlt. b%str) !&
        else
            r_less_than_str = .false.
        end if
    end function r_less_than_str

    pure elemental logical function less_than_or_equal_to_str(a, b)
        use :: strith_comparision_less
        implicit none
        class(strint_type), intent(in) :: a
        character(len=digits), intent(in) :: b

        if (is_valid(b)) then
            less_than_or_equal_to_str = (a%str .strithle. b) !&
        else
            less_than_or_equal_to_str = .false.
        end if
    end function less_than_or_equal_to_str

    pure elemental logical function r_less_than_or_equal_to_str(a, b)
        use :: strith_comparision_less
        implicit none
        character(len=digits), intent(in) :: a
        class(strint_type), intent(in) :: b

        if (is_valid(a)) then
            r_less_than_or_equal_to_str = (a .strithle. b%str) !&
        else
            r_less_than_or_equal_to_str = .false.
        end if
    end function r_less_than_or_equal_to_str

    pure elemental function max_strint_str(a, b) result(max_)
        use :: strith_comparision_max
        implicit none
        class(strint_type), intent(in) :: a
        character(len=digits), intent(in) :: b
        type(strint_type) :: max_

        max_%str = max(a%str, b)
    end function max_strint_str

    pure elemental function max_str_strint(a, b) result(max_)
        use :: strith_comparision_max
        implicit none
        character(len=digits), intent(in) :: a
        class(strint_type), intent(in) :: b
        type(strint_type) :: max_

        max_ = max_strint_str(b, a)
    end function max_str_strint

    pure elemental function min_strint_str(a, b) result(min_)
        use :: strith_comparision_min
        implicit none
        class(strint_type), intent(in) :: a
        character(len=digits), intent(in) :: b
        type(strint_type) :: min_

        min_%str = min(a%str, b)
    end function min_strint_str

    pure elemental function min_str_strint(a, b) result(min_)
        use :: strith_comparision_min
        implicit none
        character(len=digits), intent(in) :: a
        class(strint_type), intent(in) :: b
        type(strint_type) :: min_

        min_ = min_strint_str(b, a)
    end function min_str_strint
end module strith_type_strint
