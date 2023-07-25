module strith_arithmetic_unary_sign
    use :: strith_parameter
    implicit none
    private
    public :: sign

    interface sign
        procedure :: sign_str
    end interface
contains
    pure elemental character(1) function sign_str(a)
        implicit none
        character(len=digits), intent(in) :: a

        sign_str = a(sign_index:sign_index)
    end function sign_str
end module strith_arithmetic_unary_sign
