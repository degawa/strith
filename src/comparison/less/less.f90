module strith_comparision_less
    use, intrinsic :: iso_fortran_env
    use :: strith_parameter
    use :: strith_arithmetic_unary_abs
    use :: strith_comparision_core_greater
    use :: strith_comparision_core_less
    use :: strith_comparision_equal
    implicit none
    private
    public :: operator(.strithlt.)
    public :: operator(.strithle.)

    interface operator(.strithlt.)
        procedure :: is_less_than
    end interface

    interface operator(.strithle.)
        procedure :: is_less_than_or_equal_to
    end interface
contains
    ! returns `.true.` if `a < b`
    pure elemental function is_less_than(a, b) result(lt)
        implicit none
        character(len=digits), intent(in) :: a
        character(len=digits), intent(in) :: b
        logical :: lt

        if (a(sign_index:sign_index) == plus_sign) then
            if (b(sign_index:sign_index) == minus_sign) then
                lt = .false.
                return
            end if

            ! a < b where a >= 0 and b >= 0
            lt = is_less_without_considering_sign(a, b)

        else if (a(sign_index:sign_index) == minus_sign) then
            if (b(sign_index:sign_index) == plus_sign) then
                lt = .true.
                return
            end if

            ! |a| > |b| where a < 0 and b < 0
            lt = is_greater_without_considering_sign(abs(a), abs(b))
        end if
    end function is_less_than

    ! returns `.true.` if `a <= b`
    pure elemental function is_less_than_or_equal_to(a, b) result(le)
        implicit none
        character(len=digits), intent(in) :: a
        character(len=digits), intent(in) :: b
        logical :: le

        le = is_less_than(a, b) .or. (a.stritheq.b)
    end function is_less_than_or_equal_to
end module strith_comparision_less
