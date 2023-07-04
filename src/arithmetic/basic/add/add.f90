module strith_arithmetic_basic_add
    use, intrinsic :: iso_fortran_env
    use :: strith_parameter
    implicit none
    private
    public :: operator(+)

    interface operator(+)
        procedure :: add_str_str
    end interface
contains
    !>Returns the string containing the result of `a + b`.
    function add_str_str(a, b) result(add)
        implicit none
        character(len=digits), intent(in) :: a
        character(len=digits), intent(in) :: b
        character(len=digits) :: add

        integer(int32) :: carry
        integer(int32) :: num_at_d_in_a, num_at_d_in_b, added_at_d
        integer(int32) :: d

        add = zero
        carry = 0

        do d = rightmost_digit_index, leftmost_digit_index, increment_from_right_to_left
            read (a(d:d), '(I1)') num_at_d_in_a
            read (b(d:d), '(I1)') num_at_d_in_b
            added_at_d = num_at_d_in_a + num_at_d_in_b + carry

            if (added_at_d >= 10) then
                carry = 1
                added_at_d = added_at_d - 10
            else
                carry = 0
            end if

            write (add(d:d), '(I1)') added_at_d
        end do
    end function add_str_str
end module strith_arithmetic_basic_add
