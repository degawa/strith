module strith_arithmetic_basic_core_sub
    use, intrinsic :: iso_fortran_env
    use :: strith_parameter
    implicit none
    private
    public :: sub_core

contains
    function sub_core(a, b) result(sub)
        implicit none
        character(len=digits), intent(in) :: a
        character(len=digits), intent(in) :: b
        character(len=digits) :: sub

        integer(int32) :: borrow
        integer(int32) :: num_at_d_in_a, num_at_d_in_b, sub_at_d
        integer(int32) :: d

        sub = zero
        borrow = 0

        do d = rightmost_digit_index, leftmost_digit_index, increment_from_right_to_left
            read (a(d:d), '(I1)') num_at_d_in_a
            read (b(d:d), '(I1)') num_at_d_in_b
            sub_at_d = num_at_d_in_a - num_at_d_in_b - borrow

            if (sub_at_d < 0) then
                borrow = 1
                sub_at_d = sub_at_d + 10
            else
                borrow = 0
            end if

            write (sub(d:d), '(I1)') sub_at_d
        end do
    end function sub_core
end module strith_arithmetic_basic_core_sub
