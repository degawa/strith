module strith_arithmetic_basic_sub_ge0Ge0
    use :: strith_parameter
    use :: strith_arithmetic_basic_core_sub
    use :: strith_comparision_greater
    use :: strith_comparision_less
    use :: strith_arithmetic_unary_negate
    implicit none
    private
    public :: sub_ge0_ge0

contains
    function sub_ge0_ge0(a, b) result(sub)
        implicit none
        character(len=digits), intent(in) :: a
        character(len=digits), intent(in) :: b
        character(len=digits) :: sub

        if (a.strge.b) then
            sub = sub_core(a, b)
        else if (a.strlt.b) then
            sub = -(sub_core(b, a))
        end if
    end function sub_ge0_ge0
end module strith_arithmetic_basic_sub_ge0Ge0
