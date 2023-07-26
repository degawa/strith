module strith_comparision_max
    use :: strith_parameter
    use :: strith_comparision_greater
    implicit none
    private
    public :: max

    interface max
        procedure :: max_str
    end interface
contains
    pure elemental function max_str(a, b) result(max_)
        implicit none
        character(len=digits), intent(in) :: a
        character(len=digits), intent(in) :: b
        character(len=digits) :: max_

        if (a.strithge.b) then
            max_ = a
        else
            max_ = b
        end if
    end function max_str
end module strith_comparision_max
