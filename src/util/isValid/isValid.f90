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
                       is_contained(a(sign_index:sign_index), sign_set), &
                       is_contained(a(leftmost_digit_index:rightmost_digit_index), number_set) &
                       ])
    end function is_valid

    logical function is_contained(a, set_of_characters)
        implicit none
        character(*), intent(in) :: a
        character(*), intent(in) :: set_of_characters

        is_contained = (verify(a, set_of_characters) == 0)
    end function is_contained
end module strith_util_isValid
