module strith_arithmetic_basic_add
    use, intrinsic :: iso_fortran_env
    use :: strith_parameter
    use :: strith_arithmetic_basic_add_ge0Ge0
    use :: strith_arithmetic_basic_add_ge0Lt0
    use :: strith_arithmetic_basic_add_lt0Ge0
    use :: strith_arithmetic_basic_add_lt0Lt0
    use :: strith_comparision_greater
    use :: strith_comparision_less
    implicit none
    private
    public :: operator(+)

    interface operator(+)
        procedure :: add_interface
    end interface
contains
    function add_interface(a, b) result(add)
        implicit none
        character(len=digits), intent(in) :: a
        character(len=digits), intent(in) :: b
        character(len=digits) :: add

        if ((a.strge.zero) .and. (b.strge.zero)) then
            add = add_ge0_ge0(a, b)
            return
        end if

        if ((a.strlt.zero) .and. (b.strlt.zero)) then
            add = add_lt0_lt0(a, b)
            return
        end if

        if ((a.strge.zero) .and. (b.strlt.zero)) then
            add = add_ge0_lt0(a, b)
            return
        end if

        if ((a.strlt.zero) .and. (b.strge.zero)) then
            add = add_lt0_ge0(a, b)
            return
        end if
    end function add_interface
end module strith_arithmetic_basic_add
