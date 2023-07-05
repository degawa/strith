module strith_comparision_greater
    use, intrinsic :: iso_fortran_env
    use :: strith_parameter
    use :: strith_arithmetic_unary_abs
    use :: strith_comparision_core_greater
    use :: strith_comparision_core_less
    use :: strith_comparision_equal
    implicit none
    private
    public :: operator(.strithgt.)
    public :: operator(.strithge.)

    interface operator(.strithgt.)
        procedure :: is_greater_than
    end interface

    interface operator(.strithge.)
        procedure :: is_greater_than_or_equal_to
    end interface
contains
    ! returns `.true.` if `a > b`
    function is_greater_than(a, b) result(gt)
        implicit none
        character(len=digits), intent(in) :: a
        character(len=digits), intent(in) :: b
        logical :: gt

        if (a(sign_index:sign_index) == plus_sign) then
            if (b(sign_index:sign_index) == minus_sign) then
                gt = .true.
                return
            end if

            ! a > b where a >= 0 and b >= 0
            gt = is_greater_without_considering_sign(a, b)

        else if (a(sign_index:sign_index) == minus_sign) then
            if (b(sign_index:sign_index) == plus_sign) then
                gt = .false.
                return
            end if

            ! |a| < |b| where a < 0 and b < 0
            gt = is_less_without_considering_sign(abs(a), abs(b))
        end if
    end function is_greater_than

    ! returns `.true.` if `a >= b`
    function is_greater_than_or_equal_to(a, b) result(ge)
        implicit none
        character(len=digits), intent(in) :: a
        character(len=digits), intent(in) :: b
        logical :: ge

        ge = is_greater_than(a, b) .or. (a.stritheq.b)
    end function is_greater_than_or_equal_to
end module strith_comparision_greater
