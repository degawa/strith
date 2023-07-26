module strith
    use :: strith_parameter
    use :: strith_arithmetic_basic_add
    use :: strith_arithmetic_basic_sub
    use :: strith_arithmetic_unary_negate
    use :: strith_arithmetic_unary_abs
    use :: strith_arithmetic_unary_sign
    use :: strith_comparision_equal
    use :: strith_comparision_greater
    use :: strith_comparision_less
    use :: strith_comparision_max
    use :: strith_comparision_min
    use :: strith_util_toString
    use :: strith_util_toInt
    use :: strith_util_isValid
    use :: strith_util_removeZeroPadding
    use :: strith_type_strint
    implicit none
    private
    ! user-defined type
    public :: strint_type, new_strint

    ! utility procedures
    public :: to_string
    public :: to_int8
    public :: to_int16
    public :: to_int32
    public :: to_int64
    public :: is_valid
    public :: remove_zero_padding

    ! operators
    public :: operator(+)
    public :: operator(-)
    public :: abs
    public :: sign

    ! comparators
    public :: operator(.stritheq.)
    public :: operator(.strithne.)
    public :: operator(.strithgt.)
    public :: operator(.strithge.)
    public :: operator(.strithlt.)
    public :: operator(.strithle.)
    public :: min, max

    ! parameters
    public :: weights_of_digits
    public :: digits
    public :: zero, one

    ! interfaces
    public :: Iudt_to_string
end module strith
