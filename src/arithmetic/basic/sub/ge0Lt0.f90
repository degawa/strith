module strith_arithmetic_basic_sub_ge0Lt0
    use :: strith_parameter
    use :: strith_arithmetic_basic_core_add
    use :: strith_arithmetic_op_unary_abs
    implicit none
    private
    public :: sub_ge0_lt0

contains
    function sub_ge0_lt0(a, b) result(sub)
        implicit none
        character(len=digits), intent(in) :: a
        character(len=digits), intent(in) :: b
        character(len=digits) :: sub

        sub = add_core(a, abs(b))
    end function sub_ge0_lt0
end module strith_arithmetic_basic_sub_ge0Lt0
