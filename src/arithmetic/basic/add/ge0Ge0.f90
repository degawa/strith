module strith_arithmetic_basic_add_ge0Ge0
    use :: strith_parameter
    use :: strith_arithmetic_basic_core_add
    implicit none
    private
    public :: add_ge0_ge0

contains
    function add_ge0_ge0(a, b) result(add)
        implicit none
        character(len=digits), intent(in) :: a
        character(len=digits), intent(in) :: b
        character(len=digits) :: add

        add = add_core(a, b)
    end function add_ge0_ge0
end module strith_arithmetic_basic_add_ge0Ge0
