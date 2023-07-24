module strith_arithmetic_basic_add_lt0Ge0
    use :: strith_parameter
    use :: strith_arithmetic_unary_abs
    use :: strith_arithmetic_basic_sub_ge0Ge0
    implicit none
    private
    public :: add_lt0_ge0

contains
    pure elemental function add_lt0_ge0(a, b) result(add)
        implicit none
        character(len=digits), intent(in) :: a
        character(len=digits), intent(in) :: b
        character(len=digits) :: add

        add = sub_ge0_ge0(b, abs(a))
    end function add_lt0_ge0
end module strith_arithmetic_basic_add_lt0Ge0
