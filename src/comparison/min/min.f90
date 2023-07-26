module strith_comparision_min
    use :: strith_parameter
    use :: strith_comparision_less
    use :: strith_util_isValid
    implicit none
    private
    public :: min

    interface min
        procedure :: min_str
    end interface
contains
    pure elemental function min_str(a, b) result(min_)
        implicit none
        character(len=digits), intent(in) :: a
        character(len=digits), intent(in) :: b
        character(len=digits) :: min_

        if (is_valid(a) .and. is_valid(b)) then
            if (a.strithle.b) then
                min_ = a
            else
                min_ = b
            end if
        else
            min_ = zero
        end if
    end function min_str
end module strith_comparision_min
