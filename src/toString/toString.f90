module strith_toString
    use, intrinsic :: iso_fortran_env
    use :: strith_parameter
    use :: strith_arithmetic_basic_add
    use :: strith_arithmetic_basic_sub
    use :: strith_util_removeZeroPadding
    implicit none
    private
    public :: to_string

    interface to_string
        procedure :: to_string_int8
        procedure :: to_string_int16
        procedure :: to_string_int32
        procedure :: to_string_int64
    end interface

    logical, private, parameter :: remove_0_padding_default = .false.
    logical, private, parameter :: as_unsigned_default = .false.
contains
    function to_string_int8(i8, remove_0_padding, as_unsigned) result(str)
        implicit none
        integer(int8), intent(in) :: i8
        logical, intent(in), optional :: remove_0_padding
        logical, intent(in), optional :: as_unsigned
        character(:), allocatable :: str

        integer(int32) :: i, bit

        allocate (str, source=zero)

        do i = 0, bit_size(i8) - 2
            bit = ibits(i8, pos=i, len=1)
            if (bit == 1) then
                str = weights_of_digits(i) + str
            end if
        end do
        ! most significant bit
        i = bit_size(i8) - 1
        bit = ibits(i8, pos=i, len=1)
        if (optval(as_unsigned, as_unsigned_default)) then
            if (bit == 1) str = weights_of_digits(i) + str
        else
            if (bit == 1) str = str - weights_of_digits(i)
        end if

        if (optval(remove_0_padding, remove_0_padding_default)) then
            str = remove_zero_padding(str)
        end if
    end function to_string_int8

    function to_string_int16(i16, remove_0_padding, as_unsigned) result(str)
        implicit none
        integer(int16), intent(in) :: i16
        logical, intent(in), optional :: remove_0_padding
        logical, intent(in), optional :: as_unsigned
        character(:), allocatable :: str

        integer(int32) :: i, bit

        allocate (str, source=zero)

        do i = 0, bit_size(i16) - 2
            bit = ibits(i16, pos=i, len=1)
            if (bit == 1) then
                str = weights_of_digits(i) + str
            end if
        end do
        ! most significant bit
        i = bit_size(i16) - 1
        bit = ibits(i16, pos=i, len=1)
        if (optval(as_unsigned, as_unsigned_default)) then
            if (bit == 1) str = weights_of_digits(i) + str
        else
            if (bit == 1) str = str - weights_of_digits(i)
        end if

        if (optval(remove_0_padding, remove_0_padding_default)) then
            str = remove_zero_padding(str)
        end if
    end function to_string_int16

    function to_string_int32(i32, remove_0_padding, as_unsigned) result(str)
        implicit none
        integer(int32), intent(in) :: i32
        logical, intent(in), optional :: remove_0_padding
        logical, intent(in), optional :: as_unsigned
        character(:), allocatable :: str

        integer(int32) :: i, bit

        allocate (str, source=zero)

        do i = 0, bit_size(i32) - 2
            bit = ibits(i32, pos=i, len=1)
            if (bit == 1) then
                str = weights_of_digits(i) + str
            end if
        end do
        ! most significant bit
        i = bit_size(i32) - 1
        bit = ibits(i32, pos=i, len=1)
        if (optval(as_unsigned, as_unsigned_default)) then
            if (bit == 1) str = weights_of_digits(i) + str
        else
            if (bit == 1) str = str - weights_of_digits(i)
        end if

        if (optval(remove_0_padding, remove_0_padding_default)) then
            str = remove_zero_padding(str)
        end if
    end function to_string_int32

    function to_string_int64(i64, remove_0_padding, as_unsigned) result(str)
        implicit none
        integer(int64), intent(in) :: i64
        logical, intent(in), optional :: remove_0_padding
        logical, intent(in), optional :: as_unsigned
        character(:), allocatable :: str

        integer(int32) :: i, bit

        allocate (str, source=zero)

        do i = 0, bit_size(i64) - 2
            bit = int(ibits(i64, pos=i, len=1), kind=int32)
            if (bit == 1) then
                str = weights_of_digits(i) + str
            end if
        end do
        ! most significant bit
        i = bit_size(i64) - 1
        bit = int(ibits(i64, pos=i, len=1), kind=int32)
        if (optval(as_unsigned, as_unsigned_default)) then
            if (bit == 1) str = weights_of_digits(i) + str
        else
            if (bit == 1) str = str - weights_of_digits(i)
        end if

        if (optval(remove_0_padding, remove_0_padding_default)) then
            str = remove_zero_padding(str)
        end if
    end function to_string_int64

    logical function optval(x, default)
        implicit none
        logical, intent(in), optional :: x
        logical, intent(in) :: default

        if (present(x)) then
            optval = x
        else
            optval = default
        end if
    end function optval
end module strith_toString
