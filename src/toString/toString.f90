module strith_toString
    use, intrinsic :: iso_fortran_env
    use :: strith_parameter
    use :: strith_arithmetic_basic_add
    implicit none
    private
    public :: to_string

    interface to_string
        procedure :: to_string_int8
        procedure :: to_string_int16
        procedure :: to_string_int32
        procedure :: to_string_int64
    end interface

contains
    function to_string_int8(i8, remove_heading_0) result(str)
        implicit none
        integer(int8), intent(in) :: i8
        logical, intent(in), optional :: remove_heading_0
        character(:), allocatable :: str

        integer(int32) :: i, bit

        allocate (str, source=zero)

        do i = 0, bit_size(i8) - 1
            bit = ibits(i8, pos=i, len=1)
            if (bit == 1) then
                str = weights_of_digits(i) + str
            end if
        end do

        if (present(remove_heading_0)) then
            if (remove_heading_0) then
                block
                    integer(int32) :: pos_0
                    pos_0 = index(str, "0")
                    do while (pos_0 == leftmost_digit_index)
                        str = str(sign_index:sign_index)//str(leftmost_digit_index + 1:)
                        pos_0 = index(str, "0")
                    end do
                    if (str(sign_index:sign_index) == "+") str = str(leftmost_digit_index:)
                end block
            end if
        end if
    end function to_string_int8

    function to_string_int16(i16, remove_heading_0) result(str)
        implicit none
        integer(int16), intent(in) :: i16
        logical, intent(in), optional :: remove_heading_0
        character(:), allocatable :: str

        integer(int32) :: i, bit

        allocate (str, source=zero)

        do i = 0, bit_size(i16) - 1
            bit = ibits(i16, pos=i, len=1)
            if (bit == 1) then
                str = weights_of_digits(i) + str
            end if
        end do

        if (present(remove_heading_0)) then
            if (remove_heading_0) then
                block
                    integer(int32) :: pos_0
                    pos_0 = index(str, "0")
                    do while (pos_0 == leftmost_digit_index)
                        str = str(sign_index:sign_index)//str(leftmost_digit_index + 1:)
                        pos_0 = index(str, "0")
                    end do
                    if (str(sign_index:sign_index) == "+") str = str(leftmost_digit_index:)
                end block
            end if
        end if
    end function to_string_int16

    function to_string_int32(i32, remove_heading_0) result(str)
        implicit none
        integer(int32), intent(in) :: i32
        logical, intent(in), optional :: remove_heading_0
        character(:), allocatable :: str

        integer(int32) :: i, bit

        allocate (str, source=zero)

        do i = 0, bit_size(i32) - 1
            bit = ibits(i32, pos=i, len=1)
            if (bit == 1) then
                str = weights_of_digits(i) + str
            end if
        end do

        if (present(remove_heading_0)) then
            if (remove_heading_0) then
                block
                    integer(int32) :: pos_0
                    pos_0 = index(str, "0")
                    do while (pos_0 == leftmost_digit_index)
                        str = str(sign_index:sign_index)//str(leftmost_digit_index + 1:)
                        pos_0 = index(str, "0")
                    end do
                    if (str(sign_index:sign_index) == "+") str = str(leftmost_digit_index:)
                end block
            end if
        end if
    end function to_string_int32

    function to_string_int64(i64, remove_heading_0) result(str)
        implicit none
        integer(int64), intent(in) :: i64
        logical, intent(in), optional :: remove_heading_0
        character(:), allocatable :: str

        integer(int32) :: i, bit

        allocate (str, source=zero)

        do i = 0, bit_size(i64) - 1
            bit = int(ibits(i64, pos=i, len=1), kind=int32)
            if (bit == 1) then
                str = weights_of_digits(i) + str
            end if
        end do

        if (present(remove_heading_0)) then
            if (remove_heading_0) then
                block
                    integer(int32) :: pos_0
                    pos_0 = index(str, "0")
                    do while (pos_0 == leftmost_digit_index)
                        str = str(sign_index:sign_index)//str(leftmost_digit_index + 1:)
                        pos_0 = index(str, "0")
                    end do
                    if (str(sign_index:sign_index) == "+") str = str(leftmost_digit_index:)
                end block
            end if
        end if
    end function to_string_int64
end module strith_toString
