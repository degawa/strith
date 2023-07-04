module strith_validate
    use, intrinsic :: iso_fortran_env
    use :: strith_parameter
    implicit none
    private
    public :: validate

contains
    logical function validate(a)
        implicit none
        character(len=digits), intent(in) :: a
        character(:), allocatable :: sign_set, number_set
        integer(int32) :: c

        sign_set = ""
        do c = lbound(sign_symbols, dim=1), ubound(sign_symbols, dim=1)
            sign_set = sign_set//sign_symbols(c)
        end do

        number_set = ""
        do c = lbound(number_symbols, dim=1), ubound(number_symbols, dim=1)
            number_set = number_set//number_symbols(c)
        end do

        validate = all([ &
                       verify(a(sign_index:sign_index), sign_set) == 0, &
                       verify(a(leftmost_digit_index:rightmost_digit_index), number_set) == 0 &
                       ])
    end function validate
end module strith_validate
