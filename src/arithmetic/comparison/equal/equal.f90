module strith_arithmetic_comparision_equal
    use :: strith_parameter
    implicit none
    private
    public :: is_equal
    public :: is_not_equal

contains
    logical function is_equal(a, b)
        implicit none
        character(len=digits), intent(in) :: a
        character(len=digits), intent(in) :: b

        is_equal = (a == b)
    end function is_equal

    logical function is_not_equal(a, b)
        implicit none
        character(len=digits), intent(in) :: a
        character(len=digits), intent(in) :: b

        is_not_equal = .not. is_equal(a, b)
    end function is_not_equal
end module strith_arithmetic_comparision_equal
