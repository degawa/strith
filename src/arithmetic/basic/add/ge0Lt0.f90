module strith_arithmetic_basic_add_ge0Lt0
    use :: strith_parameter
    use :: strith_arithmetic_basic_sub_ge0Ge0
    use :: strith_arithmetic_unary_abs
    implicit none
    private
    public :: add_ge0_lt0

contains
    pure elemental function add_ge0_lt0(a, b) result(add)
        implicit none
        character(len=digits), intent(in) :: a
        character(len=digits), intent(in) :: b
        character(len=digits) :: add

        add = sub_ge0_ge0(a, abs(b))
    end function add_ge0_lt0
end module strith_arithmetic_basic_add_ge0Lt0
