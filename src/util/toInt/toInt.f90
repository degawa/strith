module strith_util_toInt
    use, intrinsic :: iso_fortran_env
    use :: strith_parameter
    use :: strith_util_isValid
    use :: strith_arithmetic_unary_negate
    use :: strith_arithmetic_basic_sub
    use :: strith_comparision_less
    implicit none
    private
    public :: to_int8, to_int16, to_int32, to_int64

contains
    pure elemental function to_int8(str) result(i8)
        implicit none
        character(len=digits), intent(in) :: str
        integer(int8) :: i8

        i8 = 0
        if (is_valid(str)) then
            if ((-weights_of_digits(7) .strithle. str) .and. &
                (str .strithlt. weights_of_digits(7))) then !&
                read (str, *) i8
            end if
        end if
    end function to_int8

    pure elemental function to_int16(str) result(i16)
        implicit none
        character(len=digits), intent(in) :: str
        integer(int16) :: i16

        i16 = 0
        if (is_valid(str)) then
            if ((-weights_of_digits(15) .strithle. str) .and. &
                (str .strithlt. weights_of_digits(15))) then !&
                read (str, *) i16
            end if
        end if
    end function to_int16

    pure elemental function to_int32(str) result(i32)
        implicit none
        character(len=digits), intent(in) :: str
        integer(int32) :: i32

        i32 = 0
        if (is_valid(str)) then
            if ((-weights_of_digits(31) .strithle. str) .and. &
                (str .strithlt. weights_of_digits(31))) then !&
                read (str, *) i32
            end if
        end if
    end function to_int32

    pure elemental function to_int64(str) result(i64)
        implicit none
        character(len=digits), intent(in) :: str
        integer(int64) :: i64

        i64 = 0
        if (is_valid(str)) then
            if ((-weights_of_digits(63) .strithle. str) .and. &
                (str .strithlt. weights_of_digits(63))) then !&
                read (str, *) i64
            end if
        end if
    end function to_int64
end module strith_util_toInt
