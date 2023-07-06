program to_string_udt
    use, intrinsic :: iso_fortran_env
    use :: strith
    implicit none

    type :: long
        integer(int32) :: upper
        integer(int32) :: lower
    end type

    type(long) :: l
    l%lower = int(Z"FFFFFFFF")
    l%upper = int(Z"FFFFFFFF")

    print *, to_string(l, udt_to_string, remove_0_padding=.true., as_unsigned=.true.) ! 18446744073709551615
    print *, to_string(l, udt_to_string, remove_0_padding=.true.) ! -1
contains
    subroutine udt_to_string(var, as_unsigned, str)
        implicit none
        class(*), intent(in) :: var
        logical, intent(in) :: as_unsigned
        character(len=digits), intent(inout) :: str

        integer(int32) :: i, bit

        str = zero
        select type (var); type is (long)
            ! lower 32 bits
            do i = 0, bit_size(var%lower) - 1
                bit = ibits(var%lower, pos=i, len=1)
                if (bit == 1) then
                    str = weights_of_digits(i) + str
                end if
            end do
            ! upper 31 bits
            do i = 0, bit_size(var%upper) - 2
                bit = ibits(var%upper, pos=i, len=1)
                if (bit == 1) then
                    str = weights_of_digits(i + bit_size(var%lower)) + str
                end if
            end do
            ! most significant bit
            i = bit_size(var%upper) - 1
            bit = ibits(var%upper, pos=i, len=1)
            if (as_unsigned) then
                if (bit == 1) str = weights_of_digits(i + bit_size(var%lower)) + str
            else
                if (bit == 1) str = str - weights_of_digits(i + bit_size(var%lower))
            end if
        end select
    end subroutine udt_to_string
end program to_string_udt
