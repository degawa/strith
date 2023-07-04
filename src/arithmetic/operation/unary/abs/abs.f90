module strith_arithmetic_op_unary_abs
    use :: strith_parameter
    implicit none
    private
    public :: abs

    interface abs
        procedure :: abs_str
    end interface
contains
    function abs_str(a) result(abs_a)
        implicit none
        character(len=digits), intent(in) :: a
        character(len=digits) :: abs_a

        abs_a = a
        abs_a(sign_index:sign_index) = "+"
    end function abs_str
end module strith_arithmetic_op_unary_abs
