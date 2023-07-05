module strith_arithmetic_unary_negate
    use :: strith_parameter
    implicit none
    private
    public :: operator(-)

    interface operator(-)
        procedure :: negate_str
    end interface
contains
    function negate_str(a) result(minus_a)
        implicit none
        character(len=digits), intent(in) :: a
        character(len=digits) :: minus_a

        minus_a = a

        select case (minus_a(sign_index:sign_index))
        case (sign_symbols(1))
            minus_a(sign_index:sign_index) = sign_symbols(2)

        case (sign_symbols(2))
            minus_a(sign_index:sign_index) = sign_symbols(1)

        case default
            minus_a = zero
        end select
    end function negate_str
end module strith_arithmetic_unary_negate
