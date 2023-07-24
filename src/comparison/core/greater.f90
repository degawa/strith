module strith_comparision_core_greater
    use, intrinsic :: iso_fortran_env
    use :: strith_parameter
    use :: strith_arithmetic_unary_abs
    implicit none
    private
    public :: is_greater_without_considering_sign

contains
    pure elemental function is_greater_without_considering_sign(a, b) result(is_greater)
        implicit none
        character(len=digits), intent(in) :: a
        character(len=digits), intent(in) :: b
        logical :: is_greater

        integer(int32) :: d, num_at_d_in_a, num_at_d_in_b

        is_greater = .false.
        do d = leftmost_digit_index, rightmost_digit_index, increment_from_left_to_right
            read (a(d:d), '(I1)') num_at_d_in_a
            read (b(d:d), '(I1)') num_at_d_in_b

            if (num_at_d_in_a > num_at_d_in_b) then
                is_greater = .true.
                return

            else if (num_at_d_in_a < num_at_d_in_b) then
                is_greater = .false.
                return
            else if (num_at_d_in_a == num_at_d_in_b) then
                cycle
            end if
        end do
    end function is_greater_without_considering_sign
end module strith_comparision_core_greater
