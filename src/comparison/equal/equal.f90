module strith_comparision_equal
    use :: strith_parameter
    implicit none
    private
    public :: operator(.streq.)
    public :: operator(.strne.)

    interface operator(.streq.)
        procedure :: is_equal
    end interface
    interface operator(.strne.)
        procedure :: is_not_equal
    end interface
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
end module strith_comparision_equal
