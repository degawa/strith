module strith_arithmetic_basic_sub
    use, intrinsic :: iso_fortran_env
    use :: strith_parameter
    use :: strith_arithmetic_comparision_greater
    use :: strith_arithmetic_comparision_less
    use :: strith_arithmetic_basic_sub_ge0Ge0
    use :: strith_arithmetic_basic_sub_ge0Lt0
    use :: strith_arithmetic_basic_sub_lt0Ge0
    use :: strith_arithmetic_basic_sub_lt0Lt0
    implicit none
    private
    public :: operator(-)

    interface operator(-)
        procedure :: sub_interface
    end interface

contains
    function sub_interface(a, b) result(sub)
        implicit none
        character(len=digits), intent(in) :: a
        character(len=digits), intent(in) :: b
        character(len=digits) :: sub

        if ((a.strge.zero) .and. (b.strge.zero)) then
            sub = sub_ge0_ge0(a, b)
            return
        end if

        if ((a.strlt.zero) .and. (b.strlt.zero)) then
            sub = sub_lt0_lt0(a, b)
            return
        end if

        if ((a.strge.zero) .and. (b.strlt.zero)) then
            sub = sub_ge0_lt0(a, b)
            return
        end if

        if ((a.strlt.zero) .and. (b.strge.zero)) then
            sub = sub_lt0_ge0(a, b)
            return
        end if
    end function sub_interface
end module strith_arithmetic_basic_sub
