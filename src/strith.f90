module strith
    use :: strith_parameter
    use :: strith_arithmetic_basic_add
    use :: strith_arithmetic_basic_sub
    use :: strith_arithmetic_unary_negate
    use :: strith_arithmetic_unary_abs
    use :: strith_comparision_equal
    use :: strith_comparision_greater
    use :: strith_comparision_less
    use :: strith_util_toString
    use :: strith_util_isValid
    use :: strith_util_removeZeroPadding
    implicit none
    private
    ! utility procedures
    public :: to_string
    public :: is_valid
    public :: remove_zero_padding

    ! operators
    public :: operator(+)
    public :: operator(-)
    public :: abs

    ! comparators
    public :: operator(.stritheq.)
    public :: operator(.strithne.)
    public :: operator(.strithgt.)
    public :: operator(.strithge.)
    public :: operator(.strithlt.)
    public :: operator(.strithle.)

    ! parameters
    public :: weights_of_digits
    public :: digits
    public :: zero
end module strith
