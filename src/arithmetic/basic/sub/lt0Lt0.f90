module strith_arithmetic_basic_sub_lt0Lt0
    use :: strith_parameter
    use :: strith_arithmetic_unary_abs
    use :: strith_arithmetic_basic_sub_ge0Ge0
    implicit none
    private
    public :: sub_lt0_lt0

contains
    pure elemental function sub_lt0_lt0(a, b) result(sub)
        implicit none
        character(len=digits), intent(in) :: a
        character(len=digits), intent(in) :: b
        character(len=digits) :: sub

        sub = sub_ge0_ge0(abs(b), abs(a))
    end function sub_lt0_lt0
end module strith_arithmetic_basic_sub_lt0Lt0
