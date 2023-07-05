module strith_arithmetic_basic_sub_lt0Ge0
    use :: strith_parameter
    use :: strith_arithmetic_basic_core_add
    use :: strith_arithmetic_op_unary_abs
    use :: strith_arithmetic_op_unary_negate
    implicit none
    private
    public :: sub_lt0_ge0

contains
    function sub_lt0_ge0(a, b) result(sub)
        implicit none
        character(len=digits), intent(in) :: a
        character(len=digits), intent(in) :: b
        character(len=digits) :: sub

        sub = -(add_core(abs(a), b))
    end function sub_lt0_ge0
end module strith_arithmetic_basic_sub_lt0Ge0
