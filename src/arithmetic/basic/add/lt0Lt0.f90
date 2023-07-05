module strith_arithmetic_basic_add_lt0Lt0
    use :: strith_parameter
    use :: strith_arithmetic_basic_core_add
    use :: strith_arithmetic_op_unary_abs
    use :: strith_arithmetic_op_unary_negate
    implicit none
    private
    public :: add_lt0_lt0

contains
    function add_lt0_lt0(a, b) result(add)
        implicit none
        character(len=digits), intent(in) :: a
        character(len=digits), intent(in) :: b
        character(len=digits) :: add

        add = -(add_core(abs(a), abs(b)))
    end function add_lt0_lt0
end module strith_arithmetic_basic_add_lt0Lt0
