module strith_util_isValid
    use, intrinsic :: iso_fortran_env
    use :: strith_parameter
    implicit none
    private
    public :: is_valid

contains
    logical function is_valid(a)
        implicit none
        character(len=digits), intent(in) :: a

        is_valid = all([ &
                       verify(a(sign_index:sign_index), sign_set) == 0, &
                       verify(a(leftmost_digit_index:rightmost_digit_index), number_set) == 0 &
                       ])
    end function is_valid
end module strith_util_isValid
