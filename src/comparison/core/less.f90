module strith_comparision_core_less
    use, intrinsic :: iso_fortran_env
    use :: strith_parameter
    use :: strith_arithmetic_unary_abs
    implicit none
    private
    public :: is_less_without_considering_sign

contains
    function is_less_without_considering_sign(a, b) result(is_less)
        implicit none
        character(len=digits), intent(in) :: a
        character(len=digits), intent(in) :: b
        logical :: is_less

        integer(int32) :: d, num_at_d_in_a, num_at_d_in_b

        is_less = .false.
        do d = leftmost_digit_index, rightmost_digit_index, increment_from_left_to_right
            read (a(d:d), '(I1)') num_at_d_in_a
            read (b(d:d), '(I1)') num_at_d_in_b

            if (num_at_d_in_a < num_at_d_in_b) then
                is_less = .true.
                return

            else if (num_at_d_in_a > num_at_d_in_b) then
                is_less = .false.
                return
            else if (num_at_d_in_a == num_at_d_in_b) then
                cycle
            end if
        end do
    end function is_less_without_considering_sign
end module strith_comparision_core_less
