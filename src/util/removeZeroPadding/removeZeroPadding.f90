module strith_util_removeZeroPadding
    use, intrinsic :: iso_fortran_env
    use :: strith_parameter
    implicit none
    private
    public :: remove_zero_padding

contains
    function remove_zero_padding(a) result(a_wo_0)
        implicit none
        character(len=digits), intent(in) :: a
        character(len=digits) :: a_wo_0

        integer(int32) :: pos_0

        a_wo_0 = a
        pos_0 = index(a_wo_0, "0")
        do while (pos_0 == leftmost_digit_index)
            a_wo_0 = a_wo_0(sign_index:sign_index)//a_wo_0(leftmost_digit_index + 1:)
            pos_0 = index(a_wo_0, "0")
        end do
        if (a_wo_0(sign_index:sign_index) == "+") a_wo_0 = a_wo_0(leftmost_digit_index:)
    end function remove_zero_padding
end module strith_util_removeZeroPadding
